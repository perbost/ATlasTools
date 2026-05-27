#' @title Annotate MOFA factors with OpenAI ChatGPT
#' @name MOFAchatGPT
#' @description Build per-factor summaries from MOFA weights and request
#' expert biological interpretations from an OpenAI chat model.
#' @param model (MOFA) Trained MOFA model.
#' @param path.result (str) Output directory where annotation files are saved.
#' @param openai_model (str) OpenAI model name. Default is "gpt-4o".
#' @param openai_api_key (str) OpenAI API key. Default is Sys.getenv("OPENAI_API_KEY").
#' @param openai_base_url (str) OpenAI base URL.
#' @param llm_prompt (str) Base prompt prepended before each factor JSON.
#' @param nfeatures (int) Number of top positive and negative features per view.
#' @param file.name (str) Log filename storing prompts and responses.
#' @param timeout_sec (int) Timeout in seconds for a single request.
#' @param retries (int) Number of attempts per factor.
#' @param retry_wait_sec (int) Wait time in seconds between retries.
#' @param temperature (float) Sampling temperature.
#' @param max_tokens (int) Maximum number of output tokens.
#' @export
#' @return A named list with one LLM response per factor.
MOFAchatGPT <- function(
    model,
    path.result,
    openai_model="gpt-4o",
    openai_api_key=Sys.getenv("OPENAI_API_KEY"),
    openai_base_url="https://api.openai.com/v1",
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
    timeout_sec=300,
    retries=3,
    retry_wait_sec=5,
    temperature=0,
    max_tokens=2000
){
  if(!dir.exists(path.result)) dir.create(path.result, recursive=TRUE)

  if(!nzchar(openai_api_key)) {
    stop("Missing OpenAI API key. Set OPENAI_API_KEY or pass openai_api_key.")
  }

  K <- MOFA2::get_dimensions(model)$K
  views <- names(MOFA2::get_dimensions(model)$D)

  summary_text <- vector("list", length=K)

  for(factor in seq_len(K)){
    factor_summary <- list()

    for(view in views){
      W <- MOFA2::get_weights(model, factors=factor, views=view, as.data.frame=TRUE)
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

      factor_summary[[view]] <- list(positive=top_pos, negative=top_neg)
    }

    summary_text[[factor]] <- factor_summary
  }

  call_openai <- function(prompt_txt){
    url <- paste0(sub("/+$", "", openai_base_url), "/chat/completions")

    payload <- list(
      model=openai_model,
      temperature=temperature,
      max_tokens=as.integer(max_tokens),
      messages=list(
        list(role="user", content=prompt_txt)
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
    curl::handle_setheaders(
      handle,
      "Content-Type"="application/json",
      "Authorization"=paste("Bearer", openai_api_key)
    )

    res <- curl::curl_fetch_memory(url, handle=handle)
    txt <- rawToChar(res$content)
    obj <- jsonlite::fromJSON(txt, simplifyVector=FALSE)

    if(!is.null(obj$error)) {
      msg <- obj$error$message
      if(is.null(msg)) msg <- paste(as.character(obj$error), collapse=" ")
      stop(msg, call.=FALSE)
    }

    if(is.null(obj$choices) || length(obj$choices) == 0) {
      stop("OpenAI returned no choices.", call.=FALSE)
    }

    out <- obj$choices[[1]]$message$content
    if(is.null(out)) out <- ""
    paste(as.character(out), collapse="")
  }

  responses <- vector("list", length=K)

  old_timeout <- getOption("timeout")
  options(timeout=as.integer(timeout_sec))
  on.exit(options(timeout=old_timeout), add=TRUE)

  for(n in seq_len(K)){
    path_save <- file.path(path.result, sprintf("Factor_%i", n))
    path_chatgpt <- file.path(path_save, "CHATGPT")
    if(!dir.exists(path_chatgpt)) dir.create(path_chatgpt, recursive=TRUE)

    out_file <- file.path(path_chatgpt, file.name)

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
      ans <- tryCatch(call_openai(prompt_txt), error=function(e) e)

      if(!inherits(ans, "error")){
        llm_response_txt <- paste(as.character(ans), collapse="")
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
