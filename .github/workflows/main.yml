action "Cirrus CI Email" {
  uses = "docker://cirrusactions/email:latest"
  env = {
    APP_NAME = "Cirrus CI"
  }
  secrets = ["GITHUB_TOKEN", "MAIL_FROM", "MAIL_HOST", "MAIL_USERNAME", "MAIL_PASSWORD"]
}

