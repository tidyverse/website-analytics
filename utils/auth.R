scopes <- c(
  "https://www.googleapis.com/auth/analytics",
  "https://www.googleapis.com/auth/webmasters"
)
googleAuthR::gar_auth_configure(path = "oauth-key.json")
googleAuthR::gar_auth(email = TRUE, scopes = scopes)
