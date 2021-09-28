# author: Ipsita Data
# date: 09/28/2021
# purpose: Render .Rmd as a .md file called README.md for my repo.

rmarkdown::render(
  input="2021-9-28-Project-Blog-post.Rmd",
  output_format = "github_document",
  output_file = "README.md",
  output_options=list(html_preview=FALSE,keep_html=FALSE)
)
