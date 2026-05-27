#' @title Annotate MOFA factors with OLLAMA LLM models
#' @name OLLAMAModelAnnotation
#' @description Build per-factor summaries from MOFA weights and request
#' expert biological interpretations from an OLLAMA model.
#' @param model (MOFA) Trained MOFA model.
#' @param path.result (str) Output directory where annotation files are saved.
#' @param llm_model (str) OLLAMA model name. Default is "mistral-nemo:12b".
#' @param llm_server (str) OLLAMA server URL. Default is
#' "http://host.docker.internal:11434".
#' @param llm_prompt (str) Base prompt prepended before each factor JSON.
#' @param nfeatures (int) Number of top positive and negative features per view.
#' @param file.name (str) Log filename storing prompts and responses.
#' @param seed (int) Seed passed to chat_ollama.
#' @param timeout_sec (int) Timeout in seconds for a single request.
#' @param retries (int) Number of attempts per factor.
#' @param retry_wait_sec (int) Wait time in seconds between retries.
#' @param stream_response (bool) If TRUE, stream the Ollama response.
#' @param llm_num_predict (int) Maximum number of generated tokens (Ollama `num_predict`).
#' @param ollama_backend (str) Backend used to call Ollama. Default is "direct",
#' which calls the Ollama HTTP API and is more robust for long responses.
#' @export
#' @return A named list with one LLM response per factor.

OLLAMAModelAnnotation <- function(
    model,
    path.result,
    llm_model="mistral-nemo:12b",
    llm_server="http://host.docker.internal:11434",
    llm_prompt=paste(
      "You are a senior immunologist and bioinformatician.",
      "Task:",
      "Write a complete but expert-level biological interpretation of this Factor",
      "- Focus on immune mechanisms",
      "- Avoid generic statements",
      "- Argument based on the data, not general knowledge",
      "- Give true relevant values from the data if needed to support the argument",
      "- Avoid overinterpretation, only interpret what the data can support",
      "- Write in formal scientific English suitable for a report conclusion",
      "",
      "And please do not invent informations if data are not present here.",
      sep="\n"
    ),
    nfeatures=10,
    file.name="summary_and_prompt.txt",
    seed=123,
    timeout_sec=300,
    retries=3,
    retry_wait_sec=5,
    stream_response=TRUE,
    llm_num_predict=4096,
    ollama_backend="direct"
){

  if(!dir.exists(path.result)) dir.create(path.result, recursive=TRUE)

  K <- MOFA2::get_dimensions(model)$K
  views <- names(MOFA2::get_dimensions(model)$D)

  summary_text <- vector("list", length=K)

  for(factor in seq_len(K)){
    factor_summary <- list()

    for(view in views){
      W <- MOFA2::get_weights(model,
                              factors=factor,
                              views=view,
                              as.data.frame=TRUE)

      if(nrow(W) == 0) next

      if(grepl("RNA", view, ignore.case=TRUE)){
        W$gene <- read.table(text=as.character(W$feature), sep="_")$V1
        W$feature_label <- W$gene
      } else {
        W$feature_label <- as.character(W$feature)
      }

      W_pos <- W[W$value > 0, ]
      W_neg <- W[W$value < 0, ]

      W_pos <- W_pos[order(-W_pos$value), ]
      W_neg <- W_neg[order(W_neg$value), ]

      top_pos <- head(W_pos[, c("feature_label", "value")], nfeatures)
      top_neg <- head(W_neg[, c("feature_label", "value")], nfeatures)

      colnames(top_pos) <- c("feature", "weight")
      colnames(top_neg) <- c("feature", "weight")

      factor_summary[[view]] <- list(
        positive=top_pos,
        negative=top_neg
      )
    }

    summary_text[[factor]] <- factor_summary
  }

  ollama_backend <- match.arg(ollama_backend, c("direct", "ellmer"))

  `%||%` <- function(x, y) if(is.null(x)) y else x

  call_ollama_direct <- function(prompt_txt){
    url <- paste0(sub("/+$", "", llm_server), "/api/generate")
    payload <- list(
      model=llm_model,
      prompt=prompt_txt,
      stream=isTRUE(stream_response),
      keep_alive="60m",
      options=list(
        temperature=0,
        seed=seed,
        num_predict=as.integer(llm_num_predict)
      )
    )

    handle <- curl::new_handle()
    curl::handle_setopt(
      handle,
      post=TRUE,
      postfields=jsonlite::toJSON(payload, auto_unbox=TRUE, null="null"),
      timeout=as.integer(timeout_sec),
      connecttimeout=30
    )
    curl::handle_setheaders(handle, "Content-Type"="application/json")

    if(!isTRUE(stream_response)){
      res <- curl::curl_fetch_memory(url, handle=handle)
      txt <- rawToChar(res$content)
      obj <- jsonlite::fromJSON(txt, simplifyVector=FALSE)
      if(!is.null(obj$error)) stop(obj$error, call.=FALSE)
      return(paste(as.character(obj$response %||% ""), collapse=""))
    }

    response_parts <- character(0)
    buffer <- ""
    done <- FALSE

    parse_json <- function(txt){
      if(!nzchar(txt)) return(invisible(NULL))
      obj <- jsonlite::fromJSON(txt, simplifyVector=FALSE)
      if(!is.null(obj$error)) stop(obj$error, call.=FALSE)
      if(!is.null(obj$response)) {
        response_parts <<- c(response_parts, as.character(obj$response))
      }
      if(isTRUE(obj$done)) done <<- TRUE
      invisible(NULL)
    }

    split_complete_json <- function(txt){
      chars <- strsplit(txt, "", fixed=TRUE)[[1]]
      depth <- 0
      in_string <- FALSE
      escaped <- FALSE
      start <- NA_integer_
      objects <- character(0)

      for(i in seq_along(chars)){
        ch <- chars[[i]]

        if(isTRUE(in_string)){
          if(isTRUE(escaped)){
            escaped <- FALSE
          } else if(identical(ch, "\\")){
            escaped <- TRUE
          } else if(identical(ch, "\"")){
            in_string <- FALSE
          }
          next
        }

        if(identical(ch, "\"")){
          in_string <- TRUE
        } else if(identical(ch, "{")){
          if(depth == 0) start <- i
          depth <- depth + 1
        } else if(identical(ch, "}")){
          depth <- depth - 1
          if(depth == 0 && !is.na(start)){
            objects <- c(objects, paste(chars[start:i], collapse=""))
            start <- NA_integer_
          }
        }
      }

      rest <- if(depth > 0 && !is.na(start)) paste(chars[start:length(chars)], collapse="") else ""
      list(objects=objects, rest=rest)
    }

    curl::curl_fetch_stream(
      url,
      function(chunk){
        buffer <<- paste0(buffer, rawToChar(chunk))
        parsed <- split_complete_json(buffer)
        buffer <<- parsed$rest
        invisible(lapply(parsed$objects, parse_json))
        TRUE
      },
      handle=handle
    )

    if(nzchar(buffer)) parse_json(buffer)
    if(!done) {
      warning("Ollama stream ended before a done=true message.", call.=FALSE)
    }
    paste(response_parts, collapse="")
  }

  new_chat <- function(){
    ellmer::chat_ollama(
      base_url=llm_server,
      model=llm_model,
      seed=seed,
      api_args=list(
        temperature=0,
        keep_alive="60m",
        num_predict=as.integer(llm_num_predict)
      ),
      echo="none"
    )
  }

  extract_last_turn_text <- function(){
    tryCatch({
      turn <- chat$last_turn()
      if(!is.null(turn$text)) return(paste(as.character(turn$text), collapse=""))

      content <- turn$content
      if(is.null(content)) return("")
      if(is.character(content)) return(paste(content, collapse=""))

      if(is.list(content)) {
        parts <- character(0)
        for(part in content) {
          if(is.character(part)) {
            parts <- c(parts, paste(part, collapse=""))
          } else if(is.list(part) && !is.null(part$text)) {
            parts <- c(parts, paste(as.character(part$text), collapse=""))
          }
        }
        return(paste(parts, collapse=""))
      }

      ""
    }, error=function(e) "")
  }

  collect_stream_text <- function(stream_obj){
    chunks <- tryCatch(coro::collect(stream_obj), error=function(e) e)
    if(inherits(chunks, "error")) return(chunks)
    paste(unlist(chunks, recursive=TRUE, use.names=FALSE), collapse="")
  }

  responses <- vector("list", length=K)

  old_timeout <- getOption("timeout")
  options(timeout=as.integer(timeout_sec))
  on.exit(options(timeout=old_timeout), add=TRUE)

  for(n in seq_len(K)){
    path_save <- file.path(path.result, sprintf("Factor_%i", n))
    path_ollama <- file.path(path_save, "OLLAMA")
    if(!dir.exists(path_ollama)) dir.create(path_ollama, recursive=TRUE)

    out_file <- file.path(path_ollama, file.name)

    json_txt <- jsonlite::toJSON(
      summary_text[[n]],
      dataframe="rows",
      pretty=FALSE,
      auto_unbox=TRUE,
      na="null"
    )

    prompt_txt <- paste(
      llm_prompt,
      paste0("The Factor ", n, ":"),
      json_txt,
      sep="\n"
    )

    llm_response_txt <- NULL
    last_error <- NULL

    for(attempt in seq_len(retries)){
      ans <- tryCatch(
        {
          if(ollama_backend == "direct") {
            call_ollama_direct(prompt_txt)
          } else {
            chat <- new_chat()
            if(isTRUE(stream_response)) {
              chat$stream(prompt_txt)
            } else {
              chat$chat(prompt_txt)
            }
          }
        },
        error=function(e) e
      )

      if(!inherits(ans, "error")){
        if(ollama_backend == "direct") {
          llm_response_txt <- paste(as.character(ans), collapse="")
        } else if(isTRUE(stream_response)) {
          stream_txt <- collect_stream_text(ans)
          if(inherits(stream_txt, "error")) {
            last_error <- conditionMessage(stream_txt)
            llm_response_txt <- ""
          } else {
            llm_response_txt <- stream_txt
            if(!nzchar(llm_response_txt)) llm_response_txt <- extract_last_turn_text()
          }
        } else {
          llm_response_txt <- paste(as.character(ans), collapse="")
          if(!nzchar(llm_response_txt)) llm_response_txt <- extract_last_turn_text()
        }

        if(nzchar(llm_response_txt)) break
        last_error <- "received empty text from assistant"
      } else {
        last_error <- conditionMessage(ans)
      }

      if(attempt < retries) Sys.sleep(retry_wait_sec)
    }

    if(is.null(llm_response_txt) || !nzchar(llm_response_txt)){
      llm_response_txt <- paste0(
        "LLM_ERROR after ", retries, " attempts: ",
        ifelse(is.null(last_error), "empty response", last_error)
      )
    }

    responses[[n]] <- llm_response_txt

    writeLines(
      c(
        "########## Prompt",
        prompt_txt,
        "########## Response",
        llm_response_txt,
        "___________________________________________________________",
        ""
      ),
      out_file
    )
  }

  names(responses) <- paste0("Factor_", seq_len(K))
  invisible(responses)
}
