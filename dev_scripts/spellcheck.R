library(spelling)
usethis::use_spell_check()
devtools::check()

#update wordlist
# Remove existing wordlist
file.remove("inst/WORDLIST")

# Run spell check to see all flagged words
spelling::spell_check_package()

# Create fresh wordlist with all flagged words
spelling::update_wordlist()
