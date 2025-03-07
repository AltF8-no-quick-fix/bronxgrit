# Gemini-Powered Blog Content Generation

This R script generates blog content using the Gemini API. It processes a dataset of presidential actions, creates various blog post sections (informal, critique, in-depth, reflection), and saves them as Word documents.

## Prerequisites

-   R installed
-   Required R packages: `tibble`, `dplyr`, `stringr`, `officer`, `glue`, `memoise`, `httr`, `progress`
-   A Google Gemini API key set as an environment variable `GEMINI_API_KEY`

## Installation

1.  Install the required R packages:

    ```R
    install.packages(c("tibble", "dplyr", "stringr", "officer", "glue", "memoise", "httr", "progress"))
    ```

2.  Set your Gemini API key as an environment variable. You can do this in your `.Renviron` file or directly in your R session:

    ```R
    Sys.setenv(GEMINI_API_KEY = "your_api_key_here")


# --- Configuration and Helper Functions ---

# List of available models (add or remove as needed)
available_models <- c(
  "gemini-1.5-flash-8b",
  "gemini-2.0-flash-lite",
  "gemini-2.0-pro-exp-02-05",
  "gemini-2.0-flash-exp",
  "gemini-1.5-flash",
  "gemini-1.5-pro"
)

# Memoize the gemini function
gemini_mem <- memoise(function(prompt,
                               temperature = 0.8,
                               max_output_tokens = 524,
                               api_key = Sys.getenv("GEMINI_API_KEY"),
                               safety_filter = TRUE,
                               retries = 3) {
  
  if (nchar(api_key) < 1) {
    stop("Error: GEMINI_API_KEY environment variable not set.")
  }
  last_request_time <- Sys.time() - 6 #Initialize to allow immediate first request.
  
  gemini_call <- function(model, prompt, temperature, max_output_tokens, api_key, safety_filter){
    
    # Rate Limiting (Token Bucket)
    time_since_last_request <- as.numeric(difftime(Sys.time(), last_request_time, units = "secs"))
    wait_time <- max(0, 6 - time_since_last_request)  # Ensure at least 6 seconds between requests
    if (wait_time > 0) {
      message(paste("Rate limit approaching. Waiting", round(wait_time, 2), "seconds..."))
      Sys.sleep(wait_time)
    }
    last_request_time <<- Sys.time() #Update *before* the API call, more accurate
    
    model_query <- paste0(model, ":generateContent")
    
    request_body <- list(
      contents = list(parts = list(list(text = prompt))),
      generationConfig = list(temperature = temperature, maxOutputTokens = max_output_tokens)
    )
    
    if (safety_filter) {
      request_body$safetySettings <- list(
        list(category = "HARM_CATEGORY_HARASSMENT", threshold = "BLOCK_NONE"),
        list(category = "HARM_CATEGORY_HATE_SPEECH", threshold = "BLOCK_NONE"),
        list(category = "HARM_CATEGORY_SEXUALLY_EXPLICIT", threshold = "BLOCK_NONE"),
        list(category = "HARM_CATEGORY_DANGEROUS_CONTENT", threshold = "BLOCK_NONE"),
        list(category = "HARM_CATEGORY_CIVIC_INTEGRITY", threshold = "BLOCK_NONE")
      )
    }
    response <- tryCatch({
      httr::POST(
        url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query, "?key=", api_key),
        httr::content_type_json(),
        encode = "json",
        body = request_body,
        httr::timeout(60)
      )
    }, error = function(e) {
      message(paste("Gemini API call failed:", e$message))
      return(NULL) # Return NULL on error
    })
    if (!is.null(response)) { # Check if the request was successful
      if (response$status_code > 200) {
        warning(paste("Error - ", httr::content(response, as = "text", encoding = "UTF-8")))
        return(NULL) #also return null if we get a bad status code
      }
      
      # Efficient content extraction.
      response_content <- httr::content(response)
      if (!is.null(response_content$candidates) && length(response_content$candidates) > 0) {
        output <- paste(sapply(response_content$candidates[[1]]$content$parts, `[[`, "text"), collapse = "")
      } else if (!is.null(response_content$promptFeedback)) {
        warning("Prompt feedback received: ", response_content$promptFeedback)
        output <- ""
      } else {
        output <- ""
      }
      return(output) # Return on success
    }
    return(NULL)
  }
  
  for (i in 1:retries) {
    # Randomly select a model
    current_model <- sample(available_models, 1)
    message(paste("Attempt", i, "using model:", current_model))
    output <- gemini_call(current_model, prompt, temperature, max_output_tokens, api_key, safety_filter)
    if(!is.null(output)){
      return(output)
    }
  }
  stop(paste("Gemini API call failed after", retries, "attempts and trying multiple models."))
})


    ```

## Usage

1.  **Load the script and required libraries:**

    ```R
    library(tibble)
    library(dplyr)
    library(stringr)
    library(officer)
    library(glue)
    library(memoise)
    library(httr)
    library(progress)

    # --- Configuration and Helper Functions ---
    # ... 
# --- Name Mapping  ---
name_mapping <- c(
  bannon_perspective = "Brendan",
  liberal_perspective = "Eleanor",
  conservative_perspective = "Charles",
  libertarian_perspective = "Silas",
  progressive_perspective = "Maya",
  centrist_perspective = "David",
  environmentalist_perspective = "Hazel",
  economic_perspective = "Arthur",
  globalist_perspective = "Isabella",
  human_rights_perspective = "Sofia",
  historical_perspective = "Theodore",
  legal_perspective = "Ruth",
  working_class_perspective = "Joseph",
  john_board_perspective = "John",
  alfred_noseworthy_perspective = "Alfred",
  augustine_bellweather_perspective = "Augustine"
)


# --- Main Loop and Data Handling ---

generate_blog_content <- function(final_data) {
  
  results_df <- tibble(
    title = character(),
    informal_post_prompt = character(),
    critique_prompt = character(),
    improved_post_prompt = character(),
    date = character(),
    link = character(),
    page_url = character(),
    content = character(),
    informal_post_content = character(),
    critique_content = character(),
    improved_post_content = character(),
    reflection_content = character()
  )
  
  # Define reusable prompt components INSIDE generate_blog_content
  prompt_parts <- list(
    # Tones (Shortened)
    informal_tone = "Explain simply, neutrally, with humor.  Acknowledge critics fairly.",
    wutang_tone = "Use Wu-Tang Clan style (Rza-like): ebonics, rhyme, slang.  No 'Rza' or 'Wu-Tang'.",
    formal_tone = "Formal, objective tone.  Balanced analysis for academics/professionals.",
    satirical_tone = "Satire and irony. Exaggerate to critique, but convey concerns.",
    optimistic_tone = "Optimistic, hopeful. Focus on benefits, acknowledge challenges.",
    pessimistic_tone = "Pessimistic, critical. Highlight negatives and risks.",
    urgent_tone = "Urgent, alarming. Emphasize immediate need for action/attention.",
    calm_tone = "Calm, measured. Balanced analysis, no hyperbole.",
    humorous_tone = "Humorous, witty. Approachable and entertaining, but informative.",
    cynical_tone = "Cynical, skeptical. Question motives, doubt effectiveness.",
    analytical_tone = "Analytical, data-driven. Facts, figures, logic.",
    motivational_tone = "Motivational, inspiring. Encourage action/support.",
    poetic_tone = "Poetic, evocative. Use imagery and metaphors.",
    
    # Perspectives (Shortened)
    bannon_perspective = "Populist (like Bannon). Support order. Frame for working-class, critique elites. Nationalism, sovereignty, tradition. Combative. Globalism/elites/bureaucracy = harm. Bold action needed.",
    liberal_perspective = "Liberal. Analyze social justice, equality, civil liberties impact. Critique harm to vulnerable groups/environment.",
    conservative_perspective = "Conservative. Analyze individual liberty, limited government, free markets. Critique government overreach.",
    libertarian_perspective = "Libertarian. Prioritize individual liberty, minimal government. Critique government power expansion.",
    progressive_perspective = "Progressive. Social/economic justice. Policies for marginalized, promote equality.",
    centrist_perspective = "Centrist. Balanced, moderate. Consider all sides, seek compromise.",
    environmentalist_perspective = "Environmentalist. Impact on environment, climate change, sustainability.",
    economic_perspective = "Economic implications: jobs, markets, trade, growth.",
    globalist_perspective = "International relations, global cooperation, America's world role.",
    human_rights_perspective = "Human rights impact, domestic and international.",
    historical_perspective = "Historical context. Compare to past, long-term significance.",
    legal_perspective = "Legal: constitutionality, legality, challenges.",
    working_class_perspective = "Impact on working-class families: jobs, wages, security.",
    john_board_perspective = "Analyze, BUT get distracted by unrelated, mundane thoughts (e.g., love, comfy shoes, leaky shower).  Return briefly, then distract again.",
    alfred_noseworthy_perspective = "Alfred Noseworthy: Analyze as coded messages, conspiracies. Reveal 'true' purpose, hidden agendas. Everything connected.",
    augustine_bellweather_perspective = "Prof. Augustine Bellweather: Regulatory law/environmental policy. Dissect language, cite regulations/studies, expose fallacies/agendas, historical context. Critical.",
    
    # Focuses (Shortened)
    implications_focus = "Positive and negative implications. Consider different perspectives.",
    long_term_focus = "In-depth, long-term impact. Consequences, future shaping. Include data.",
    short_term_focus = "Immediate effects and consequences.",
    political_focus = "Political ramifications: power balance, elections, discourse.",
    economic_focus_detailed = "Detailed economic consequences: industries, employment, inflation, trade.",
    social_focus = "Social impact: communities, groups, norms.",
    legal_focus_detailed = "Legal: challenges, constitutionality, precedents.",
    international_focus = "International implications: relations, agreements, stability.",
    ethical_focus = "Ethical considerations. Morality, dilemmas.",
    historical_focus_detailed = "Detailed historical analysis: precedents, place in history.",
    cultural_focus = "Cultural impact: trends, values, art.",
    technological_focus = "Technological aspects and implications.",
    psychological_focus = "Potential psychological impact.",
    
    reflection_focus = "You are {impacted_person}. How does this affect your life, family, job, community? Benefits? Drawbacks? Risks of abuse/unfairness?"
  )
  
  create_prompt <- function(type, action_info, tone = NULL, perspective = NULL, focus = NULL,
                            extra_instructions = NULL, prior_content = NULL,
                            impacted_person = NULL, detailed_input = NULL) {
    
    # More Concise Basic Structure
    base_prompt <- glue("Presidential Action: {action_info$title}\nSummary: {action_info$content}")
    
    # Build prompt
    prompt <- switch(type,
                     informal_post = paste(
                       "Write an informal blog post:", base_prompt,
                       if (!is.null(tone)) prompt_parts[[paste0(tone, "_tone")]] else prompt_parts$informal_tone,
                       extra_instructions, sep = "\n\n"
                     ),
                     critique = paste(
                       "Critique/analyze:", base_prompt,
                       if (!is.null(perspective)) prompt_parts[[paste0(perspective, "_perspective")]] else NULL,
                       if (!is.null(focus)) prompt_parts[[paste0(focus, "_focus")]] else prompt_parts$implications_focus,
                       extra_instructions, sep = "\n\n"
                     ),
                     improved_post = paste(
                       "Detailed, comprehensive analysis:", base_prompt,
                       if (!is.null(focus)) prompt_parts[[paste0(focus, "_focus")]] else prompt_parts$long_term_focus,
                       extra_instructions,
                       if (!is.null(prior_content)) paste("Building on:", prior_content, sep = "\n"),
                       sep = "\n\n"
                     ),
                     reflection = paste(
                       base_prompt,
                       prompt_parts$reflection_focus,
                       extra_instructions,
                       sep = "\n\n"
                     ),
                     impacted_person = paste0("Who is most impacted? (e.g., workers, families, immigrants).\nPolicy Summary: ", action_info$content),
                     simple_impact = paste0("Summarize impacted group (1-2 words):\n", detailed_input),
                     stop("Invalid prompt type.")
    )
    return(prompt)
  }
  
  # --- Randomization Setup ---
  # These need to be INSIDE generate_blog_content
  available_tones <- setdiff(names(prompt_parts)[grepl("_tone$", names(prompt_parts))], "informal_tone") # Exclude default
  available_perspectives <- setdiff(names(prompt_parts)[grepl("_perspective$", names(prompt_parts))], "NULL") #Get perspective names
  available_focuses <- setdiff(names(prompt_parts)[grepl("_focus$", names(prompt_parts))], c("implications_focus", "long_term_focus")) # Exclude defaults
  
  # Initialize to something that won't be chosen
  last_tone <- "initial_tone"  # Use a value not in your available tones
  last_perspective <- "initial_perspective" # Use a value not in your available perspectives
  last_focus <- "initial_focus" # Use a value not in your available focuses
  
  for (i in 1:nrow(final_data)) {
    action_info <- final_data[i, ]
    
    # --- Randomized Style Selection ---
    # Tone
    current_tone <- sample(c("informal", available_tones), 1)
    while(current_tone == last_tone) {
      current_tone <- sample(c("informal", available_tones), 1)
    }
    last_tone = current_tone
    
    # Perspective - CORRECTED SAMPLING
    current_perspective <- if (runif(1) < 0.7) {
      sample(available_perspectives, 1)
    } else {
      NULL
    }
    if (!is.null(current_perspective) && !is.null(last_perspective)) {
      while (current_perspective == last_perspective) {
        current_perspective <- if (runif(1) < 0.7) {
          sample(available_perspectives, 1)
        } else {
          NULL
        }
      }
    }
    last_perspective <- current_perspective
    
    #Focus
    current_focus <- sample(c("implications", "long_term", available_focuses), 1)
    while(current_focus == last_focus) {
      current_focus <- sample(c("implications", "long_term", available_focuses), 1)
    }
    last_focus = current_focus
    # --- End Randomized Style Selection ---
    
    
    # Step 1: Identify impacted person
    impacted_person_detailed <- gemini_mem(create_prompt("impacted_person", action_info), max_output_tokens = 150)
    impacted_person_input <- gemini_mem(create_prompt("simple_impact", action_info, detailed_input = impacted_person_detailed), max_output_tokens = 20)
    
    # Step 2: Generate Prompts
    informal_post_prompt <- create_prompt("informal_post", action_info, tone = current_tone)
    critique_prompt <- create_prompt("critique", action_info, perspective = current_perspective, focus = current_focus)
    improved_post_prompt <- create_prompt("improved_post", action_info, focus = "long_term", prior_content = paste(informal_post_prompt, critique_prompt, sep = "\n\n"))
    reflection_prompt <- create_prompt("reflection", action_info, impacted_person = impacted_person_input)
    
    # Step 3: Generate Content (with error handling) and adjusted token limits
    informal_post_content <- tryCatch({
      gemini_mem(informal_post_prompt, temperature = 0.9, max_output_tokens = 200) # Reduced
    }, error = function(e) {
      message("Error in gemini call for informal post: ", e$message)
      return(paste("Error generating informal post:", e$message))
    })
    
    critique_content <- tryCatch({
      gemini_mem(critique_prompt, temperature = 0.8, max_output_tokens = 400) # Reduced
    }, error = function(e) {
      message("Error in gemini call for critique: ", e$message)
      return(paste("Error generating critique:", e$message))
    })
    
    improved_post_content <- tryCatch({
      gemini_mem(improved_post_prompt, temperature = 0.5, max_output_tokens = 800) # Reduced
    }, error = function(e) {
      message("Error in gemini call for improved post: ", e$message)
      return(paste("Error generating improved post:", e$message))
    })
    
    reflection_content <- tryCatch({
      gemini_mem(reflection_prompt, temperature = 0.7, max_output_tokens = 300) # Reduced
    }, error = function(e) {
      message("Error in gemini call for reflection: ", e$message)
      return(paste("Error generating reflection:", e$message))
    })
    
    # Step 4: Store Results
    results_df <- results_df %>% add_row(
      title = action_info$title,
      informal_post_prompt = informal_post_prompt,
      critique_prompt = critique_prompt,
      improved_post_prompt = improved_post_prompt,
      date = as.character(
        if_else(is.na(action_info$date),
                mdy(str_extract(action_info$content, "\\w+\\s+\\d{1,2},\\s+\\d{4}")),  # Corrected regex
                action_info$date)
      ),
            link = action_info$link,
            page_url = action_info$page_url,
            content = action_info$content,
            informal_post_content = informal_post_content,
            critique_content = critique_content,
            improved_post_content = improved_post_content,
            reflection_content = reflection_content
        )
    }

    return(results_df)
}


# --- Document Generation ---

remove_incomplete_last_sentence <- function(paragraph) {
  if (is.null(paragraph) || is.na(paragraph) || !is.character(paragraph) || length(paragraph) == 0) {
    return("") # Handle NULL, NA, non-character, or empty input
  }
  if (length(paragraph) > 1) {
    return(paste(paragraph, collapse = "\n"))
  }
  
  sentences <- str_split(paragraph, "(?<=[.!?])\\s+", simplify = FALSE)[[1]] # Use simplify = FALSE
  
  if (length(sentences) == 0) {
    return("") # Return empty string if no sentences found
  }
  
  # Check if the *last* sentence is complete.  Remove it only if incomplete.
  if (!grepl("[.!?]$", trimws(sentences[length(sentences)]))) {
    sentences <- sentences[-length(sentences)]
  }
  
  return(paste(sentences, collapse = " ")) # Always return a single string
}

add_paragraphs <- function(doc, text) {
    text_parts <- str_split(text, "\n", simplify = TRUE)
    text_parts <- text_parts[nzchar(text_parts)]
    text_parts <- sapply(text_parts, remove_incomplete_last_sentence)
    body_add_par(doc, value = text_parts, style = "Normal")
    return(doc)
}

    ```

2.  **Load your data:** Ensure you have a `final_data` data frame loaded into your R environment. This data frame should contain the presidential action information (title, content, date, link, page\_url).

3.  **Run the script:** The script will process the `final_data` subset, generate blog content, and save it as Word documents in your working directory.

    ```R
    # --- Main Execution ---
    # Assume final_data is loaded

    # Set subset size
    subset_size <- 132
    final_data_subset <- final_data[1:subset_size, ]

    # Create the progress bar
    pb <- progress_bar$new(
      format = "[:bar] :percent (:eta)",
      total = nrow(final_data_subset),  # Use the subset size
      clear = FALSE,
      width = 80
    )

    # 2. Call generate_blog_content and modify critique_content
    results_df_test <- generate_blog_content(final_data_subset)

    # 3. Modify critique_content with names (using name_mapping)
    for (i in 1:nrow(results_df_test)) {
      #Extract the perspective from prompt
      current_perspective <- str_extract(results_df_test$critique_prompt[i], paste(names(name_mapping), collapse = "|"))
      if (!is.null(current_perspective) && !is.na(current_perspective)) {
        name <- name_mapping[current_perspective]
          if (!is.na(name)) {
            results_df_test$critique_content[i] <- paste0("Hello my name is: ", name, "\n", results_df_test$critique_content[i])
          }
      }
    }

    # 4. Create Word documents (for the subset)
    for (i in 1:nrow(results_df_test)) {

      action_info <- results_df_test[i, ]

      doc <- read_docx()

      doc <- doc %>%
        body_add_par(action_info$title, style = "heading 1") %>%
        body_add_par(paste("Date: ", action_info$date), style = "Normal") %>%
        body_add_par("Link: ", style = "Normal") %>%
        body_add_par(action_info$link, style = "Normal") %>%
        body_add_par("URL: ", style = "Normal") %>%
        body_add_par(action_info$page_url, style = "Normal") %>%
        body_add_par("")

      doc <- doc %>%
        body_add_par("Introduction:", style = "heading 2") %>%
        body_add_par("", style = "Normal")
      doc <- add_paragraphs(doc, action_info$informal_post_content)

      doc <- doc %>%
        body_add_par("Perspectives:", style = "heading 2") %>%
        body_add_par("", style = "Normal")
      doc <- add_paragraphs(doc, action_info$critique_content)

      doc <- doc %>%
        body_add_par("In_depth:", style = "heading 2")
      doc <- add_paragraphs(doc, action_info$improved_post_content)

      doc <- doc %>%
        body_add_par("Reflection:", style = "heading 2") %>%
        body_add_par("", style = "Normal")
      doc <- add_paragraphs(doc, action_info$reflection_content)

      file_name <- paste0("Blog_Post_", gsub("[^[:alnum:]_]", "_", substr(action_info$title, 1, 30)), ".docx")
      file_path <- file.path(getwd(), file_name)

      print(paste("Saving to", file_path))
      print(doc, target = file_path)
    }
    ```

## Script Details

-   **Gemini API Interaction:** The script uses the `httr` package to send requests to the Gemini API. It handles rate limiting and retries to ensure robustness.
-   **Prompt Engineering:** The `create_prompt` function generates prompts for different sections of the blog post, incorporating various tones, perspectives, and focuses.
-   **Content Generation:** The `generate_blog_content` function iterates through the data, generates content using the Gemini API, and stores it in a data frame.
-   **Word Document Creation:** The script uses the `officer` package to create Word documents with formatted content.
-   **Error Handling:** The script includes `tryCatch` blocks to handle potential errors during API calls.
-   **Randomized Style Selection:** The tone, perspective, and focus of the blog post are randomized to generate diverse content.
-   **Name Mapping:** The script uses a `name_mapping` vector to add names to the critique section of the blog post.

## Configuration

-   **`available_models`:** A vector of available Gemini models. You can add or remove models as needed.
-   **`name_mapping`:** A named vector mapping perspectives to names.
-   **`subset_size`:** The number of rows from `final_data` to process.

## Important Notes

-   Ensure your `GEMINI_API_KEY` is correctly set.
-   Adjust the `subset_size` and API parameters (temperature, max\_output\_tokens) as needed.
-   The script handles rate limiting, but you may need to adjust the wait time based on your API usage.
-   The script assumes your `final_data` data frame is correctly formatted.
-   The script generates Word documents in your current working directory.
-   The script includes a progress bar to show the processing progress.

## Function Descriptions

### `gemini_mem(prompt, temperature, max_output_tokens, api_key, safety_filter, retries)`

-      Sends a prompt to the Gemini API and returns the generated content.
-      Uses memoization to cache results and reduce redundant API calls.
-      Handles rate limiting and retries to ensure robustness.
-      Randomly selects a model from `available_models` for each attempt.
-      Parameters:
    -   `prompt`: The text prompt to send to the API.
    -   `temperature`: Controls the randomness of the output.
    -   `max_output_tokens`: Limits the length of the generated content.
    -   `api_key`: Your Gemini API key.
    -   `safety_filter`: Enables or disables safety filters.
    -   `retries`: Number of retry attempts.

### `generate_blog_content(final_data)`

-      Generates blog content for each row in the `final_data` data frame.
-      Creates prompts for informal posts, critiques, improved posts, and reflections.
-      Uses the `gemini_mem` function to generate content.
-      Stores the generated content and prompts in a data frame.
-      Randomizes the tone, perspective, and focus of the blog posts.
-   Parameters:
    -   `final_data`: A data frame containing presidential action information.

### `create_prompt(type, action_info, tone, perspective, focus, extra_instructions, prior_content, impacted_person, detailed_input)`

-      Creates a prompt based on the specified type, action information, tone, perspective, and focus.
-      Uses `glue` to construct the prompt with dynamic content.
-      Parameters:
    -   `type`: The type of prompt (informal\_post, critique, improved\_post, reflection, impacted\_person, simple\_impact).
    -   `action_info`: A data frame row containing action information.
    -   `tone`: The tone of the prompt.
    -   `perspective`: The perspective of the prompt.
    -   `focus`: The focus of the prompt.
    -   `extra_instructions`: Additional instructions for the prompt.
    -   `prior_content`: Content from previous prompts to build on.
    -   `impacted_person`: impacted person input.
    -   `detailed_input`: detailed input for simple impact prompt.

### `remove_incomplete_last_sentence(paragraph)`

-   Removes the last sentence of a paragraph if it is incomplete.
-   Parameters:
    -   `paragraph`: The text paragraph.
-   Returns:
    -   The text paragraph with the last sentence removed if incomplete.

### `add_paragraphs(doc, text)`

-   Adds paragraphs to a Word document.
-   Splits the text into paragraphs based on newline characters.
-   Removes incomplete last sentences from each paragraph.
-   Parameters:
    -   `doc`: The Word document object.
    -   `text`: The text to add as paragraphs.
-   Returns:
    -   The modified Word document object.

## Data Structure

### `final_data`

-      A data frame containing presidential action information.
-      Columns:
    -   `title`: The title of the action.
    -   `content`: A summary of the action.
    -   `date`: The date of the action.
    -   `link`: A link to the action.
    -   `page_url`: The URL of the action's page.

### `results_df`

-      A data frame containing generated blog content and prompts.
-      Columns:
    -   `title`: The title of the action.
    -   `informal_post_prompt`: The prompt for the informal post.
    -   `critique_prompt`: The prompt for the critique.
    -   `improved_post_prompt`: The prompt for the improved post.
    -   `date`: The date of the action.
    -   `link`: A link to the action.
    -   `page_url`: The URL of the action's page.
    -   `content`: A summary of the action.
    -   `informal_post_content`: The generated content for the informal post.
    -   `critique_content`: The generated content for the critique.
    -   `improved_post_content`: The generated content for the improved post.
    -   `reflection_content`: The generated content for the reflection.

## Error Handling

-      The script uses `tryCatch` blocks to handle errors during API calls.
-      Error messages are printed to the console.
-      The script continues processing even if an error occurs.

## Rate Limiting

-      The script implements rate limiting to avoid exceeding API usage limits.
-      It waits for a specified time between API calls.

## Retries

-   If an API call fails, the script retries the call multiple times.
-   It uses a randomized selection of available models for each retry.

## Additional Features

-   **Progress Bar:** The script uses a progress bar to show the processing progress.
-   **Memoization:** The `gemini_mem` function uses memoization to cache results and reduce redundant API calls.
-   **Randomized Style Selection:** The tone, perspective, and focus of the blog post are randomized to generate diverse content.
-   **Name Mapping:** The script uses a `name_mapping` vector to add names to the critique section of the blog post.

# Bronx Grit Oval Office Orders

This repository contains articles and analyses on various topics related to politics, policy, and current events.

## Recent Articles

* [List of Executive Orders](ovalofficeorders/content/post/)

* [Biden's First 100 Days: A Mixed Bag](ovalofficeorders/content/post/Bidens_First_100_Days.md)
* [Examining the American Rescue Plan](ovalofficeorders/content/post/Examining_the_American_Rescue_Plan_Act.md) 
* [The Filibuster: A Relic of the Past?](ovalofficeorders/content/post/The_Filibuster_A_Relic_of_the_Past.md) 
## Future Improvements

-   **More sophisticated prompt engineering:** Explore more advanced prompt techniques to improve the quality and relevance of the generated content.
-   **Fine-grained control over output length:** Implement mechanisms to control the length of each section of the blog post more precisely.
-   **Integration with other APIs:** Integrate with other APIs, such as image generation or sentiment analysis APIs, to enhance the blog content.
-   **User interface:** Develop a user interface to make the script more accessible and user-friendly.
-   **Automated publishing:** Integrate with blogging platforms to automate the publishing of generated content.

## Contributing

Contributions are welcome! Feel free to submit pull requests or open issues for bug reports or feature requests.

## License

This project is licensed under the MIT License.

## Disclaimer

This script is provided as-is and is not affiliated with Google or the Gemini team. Use it at your own risk.