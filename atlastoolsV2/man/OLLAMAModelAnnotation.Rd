% Generated manually
\name{OLLAMAModelAnnotation}
\alias{OLLAMAModelAnnotation}
\title{Annotate MOFA factors with OLLAMA LLM models}
\usage{
OLLAMAModelAnnotation(
  model,
  path.result,
  llm_model = "mistral-nemo:12b",
  llm_server = "http://host.docker.internal:11434",
  llm_prompt = paste(
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
    sep = "\\n"
  ),
  nfeatures = 10,
  file.name = "summary_and_prompt.txt",
  seed = 123,
  timeout_sec = 300,
  retries = 3,
  retry_wait_sec = 5,
  stream_response = TRUE,
  llm_num_predict = 4096,
  ollama_backend = "direct"
)
}
\arguments{
\item{model}{(MOFA) Trained MOFA model.}
\item{path.result}{(str) Output directory where annotation files are saved.}
\item{llm_model}{(str) OLLAMA model name. Default is \code{"mistral-nemo:12b"}.}
\item{llm_server}{(str) OLLAMA server URL. Default is \code{"http://host.docker.internal:11434"}.}
\item{llm_prompt}{(str) Base prompt prepended before each factor JSON.}
\item{nfeatures}{(int) Number of top positive and negative features per view.}
\item{file.name}{(str) Log filename storing prompts and responses.}
\item{seed}{(int) Seed passed to chat_ollama.}
\item{timeout_sec}{(int) Timeout in seconds for a single request.}
\item{retries}{(int) Number of attempts per factor.}
\item{retry_wait_sec}{(int) Wait time in seconds between retries.}
\item{stream_response}{(bool) If TRUE, stream the Ollama response.}
\item{llm_num_predict}{(int) Maximum number of generated tokens (Ollama \code{num_predict}).}
\item{ollama_backend}{(str) Backend used to call Ollama. Default is \code{"direct"}, which calls the Ollama HTTP API and is more robust for long responses.}
}
\value{A named list with one LLM response per factor.}
\description{Build per-factor summaries from MOFA weights and request expert biological interpretations from an OLLAMA model.}
