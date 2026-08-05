% Generated manually
\name{MOFACLAUDE}
\alias{MOFACLAUDE}
\title{Annotate MOFA factors with Anthropic Claude}
\usage{
MOFACLAUDE(
  model,
  path.result,
  anthropic_model = "claude-3-5-sonnet-20241022",
  anthropic_api_key = Sys.getenv("ANTHROPIC_API_KEY"),
  anthropic_base_url = "https://api.anthropic.com/v1",
  anthropic_version = "2023-06-01",
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
  timeout_sec = 300,
  retries = 3,
  retry_wait_sec = 5,
  temperature = 0,
  max_tokens = 2000
)
}
\arguments{
\item{model}{(MOFA) Trained MOFA model.}
\item{path.result}{(str) Output directory where annotation files are saved.}
\item{anthropic_model}{(str) Anthropic Claude model name. Default is \code{"claude-3-5-sonnet-20241022"}.}
\item{anthropic_api_key}{(str) Anthropic API key. Default is \code{Sys.getenv("ANTHROPIC_API_KEY")}.}
\item{anthropic_base_url}{(str) Anthropic base URL.}
\item{anthropic_version}{(str) Anthropic API version header.}
\item{llm_prompt}{(str) Base prompt prepended before each factor JSON.}
\item{nfeatures}{(int) Number of top positive and negative features per view.}
\item{file.name}{(str) Log filename storing prompts and responses.}
\item{timeout_sec}{(int) Timeout in seconds for a single request.}
\item{retries}{(int) Number of attempts per factor.}
\item{retry_wait_sec}{(int) Wait time in seconds between retries.}
\item{temperature}{(float) Sampling temperature.}
\item{max_tokens}{(int) Maximum number of output tokens.}
}
\value{A named list with one LLM response per factor.}
\description{Build per-factor summaries from MOFA weights and request expert biological interpretations from an Anthropic Claude model.}
