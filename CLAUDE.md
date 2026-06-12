# Code Style

- One space after commas and assignment operators (`<-`)
- `if`/`else` bodies always use `{` on the same line and the body on the next line
- Long pipelines/chains start on a new line after the assignment:
  ```r
  result <-
    data %>%
    filter(...) %>%
    mutate(...)
