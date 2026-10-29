# Storing your API key

Keys live in your personal `.Renviron` file, never in a script or in this repository.

## Step 1: Install the usethis package
Within the R Console:
`install.packages('usethis')`

## Step 2: Open your local environment file
`usethis::edit_r_environ()`

## Step 3: Add your key with this exact name
Paste the line below into the file that opens, replacing the text between the quotes with your key:

`OPENROUTER_API_KEY='sk-or-v1-...'`

## Step 4: Save, close the file and restart R

Scripts in this course read the key with:

`Sys.getenv('OPENROUTER_API_KEY')`
