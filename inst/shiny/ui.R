shinyUI(fluidPage(
  
  shiny::tabsetPanel(
    shiny::tabPanel("Deploy Model", 
                    
                    shinyjs::useShinyjs(),
                    
                    # Sidebar with a slider input for number of bins
                    shiny::sidebarLayout(
                      shiny::sidebarPanel(
                        
                        ## data_dir
                        shinyFiles::shinyDirButton("data_dir", "data_dir", 
                                                   title="Select the location of photos to be analyzed. Just select the folder where it resides in the top half of the menu and press `Select`"),
                        shinyBS::bsTooltip("data_dir", "Location of image files on your computer. If they are in sub-directories within your data_dir, select `recurse=TRUE`", 
                                           placement = "top"),
                        shiny::verbatimTextOutput("data_dir_Display", placeholder = TRUE),
                        
                        ## model_type
                        shiny::selectInput("model_type", "model_type", 
                                           choices = c("general_v1", "general_v2", "family_v1", "family_v2", 
                                                       "species_v1", "species_v2", "pig_only_v1", "pig_only_v2")),
                        shinyBS::bsTooltip("model_type", "This defines your model type (taxonomic level) and version.",
                                           placement = "top"),
                        
                        ## recurse
                        shiny::selectInput("recurse", "recurse", 
                                           choices = c(TRUE, FALSE)),
                        shinyBS::bsTooltip("recurse", "Do you have images in subfolders within data_dir?", 
                                           placement = "top"),
                        
                        ## file_extensions
                        shiny::checkboxGroupInput("file_extensions", "file_extensions",  
                                                  choices = c(".jpg", ".png", ".tif", ".pdf"), 
                                                  selected = ".jpg", 
                                                  inline = TRUE),
                        shinyBS::bsTooltip("file_extensions", "The types of extensions on your image files", 
                                           placement = "top"),
                        
                        ## make_plots
                        shiny::selectInput("make_plots", "make_plots", choices = c(TRUE, FALSE)),
                        shinyBS::bsTooltip("make_plots", "Do you want to make plots of the images with their predicted bounding boxes?", 
                                           placement = "top"),

                        ## output_dir
                        shinyFiles::shinyDirButton("output_dir", "output_dir", 
                                                   title="Select the location to output. Just select the folder where it resides in the top half of the menu and press `Select`"),
                        shinyBS::bsTooltip("output_dir", "Location for output to be written on your computer. If folder is not selected, then a folder will be created within your data_dir. To resume from a previously-saved checkpoint, this field must be populated with the folder where the checkpoint file is stored.", 
                                           placement = "top"),
                        shiny::verbatimTextOutput("output_dir_Display", placeholder = TRUE),
                        
                        ## sample50
                        shiny::selectInput("sample50", "sample50", choices = c(FALSE, TRUE)),
                        shinyBS::bsTooltip("sample50", "Do you want to run the model only on a subset of 50 images from your dataset?", 
                                           placement = "top"),
                        
                        ## write_bbox_csv
                        shiny::selectInput("write_bbox_csv", "write_bbox_csv", choices = c(TRUE, FALSE)),
                        shinyBS::bsTooltip("write_bbox_csv", "Do you want to create a csv with all of the information on predicted bounding boxes?", 
                                           placement = "top"),
                        
                        ## score_threshold
                        shiny::numericInput("score_threshold", "score_threshold", value = 0.6, 
                                            min = 0, max = 0.99, step = 0.01),
                        shinyBS::bsTooltip("score_threshold", 
                                           "Confidence threshold for using a bounding box. A lower number will produce more bboxes (it will be less stringent in deciding to make a bbox). A higher number will produce fewer bboxes (it will be more stringent).", 
                                           placement = "top"),
                        
                        ## overlap_correction
                        shiny::selectInput("overlap_correction", "overlap_correction", choices = c(TRUE, FALSE)),
                        shinyBS::bsTooltip("write_bbox_csv", "Do you want overlapping detections to be evaluated and the detection with highest confidence returned?", 
                                           placement = "top"),
                        
                        ## overlap_threshold
                        shiny::numericInput("overlap_threshold", "overlap_threshold", value = 0.9, 
                                            min = 0, max = 0.99, step = 0.01),
                        shinyBS::bsTooltip("overlap_threshold", 
                                           "Proportion of bounding box overlap to determine if detections are to be considered a single detection.",
                                           placement = "top"),
                        ## get metadata
                        shiny::selectInput("get_metadata", "get_metadata", choices = c(TRUE, FALSE)),
                        shinyBS::bsTooltip("get_metadata", "Do you want to include select image metadata fields in your output?",
                                           placement = "top"),
                        ## write metadata
                        shiny::selectInput("write_metadata", "write_metadata", choices = c(TRUE, FALSE)),
                        shinyBS::bsTooltip("write_metadata", "Do you want to write model predictions to your image metadata?",
                                           placement = "top"),
                        
                        ## checkpoint frequency
                        shiny::numericInput("checkpoint_frequency", "checkpoint_frequency", value = 10,
                                            min = 1, max = 1000, step = 1),
                        shinyBS::bsTooltip("checkpoint_frequency", 
                                           "Save your results after -xx- new images are run. Since backing up your work is crucial, we coerce this argument to checkpoint at least every 1000 images.", 
                                           placement = "top"),
                        
                        ## location
                        shiny::numericInput("latitude", "latitude", value = NULL),
                        shinyBS::bsTooltip("latitude", "The image location latitude"),
                        shiny::numericInput("longitude", "longitude", value = NULL),
                        shinyBS::bsTooltip("longitude", "The image location longitude"),
                        
                      ),
                      
                      # run model button / console output
                      shiny::mainPanel(
                        
                        # shiny::h4("Current deploy_model code: (You can copy this and paste it to the console if you prefer)"),
                        # shiny::fluidRow(column(12, textOutput("deploy_modelCode"))),
                        
                        shiny::h2("Deploy CameraTrapDetectoR Models"),
                        shiny::h3("Select your model arguments from the panel on the left."),
                        shiny::h4("See below for detailed descriptions of each argument."),
                        shiny::h4("When you're ready, hit the ", strong("Run model"), " button below:"),
                        
                        shiny::hr(),
                        
                        shiny::actionButton("deploy_modelRun", "Run model"),
                        
                        shiny::fluidRow(shiny::column(12, shiny::span(style = "color:green;font-size:125%;", 
                                                                      shiny::textOutput("allowedToDeployModel")))),
                        shiny::fluidRow(shiny::column(12, shiny::span(style = "color:red;font-size:125%;", 
                                                                      shiny::textOutput("dataDirWarning")))),
                        shiny::fluidRow(shiny::column(12, shiny::span(style = "color:red;font-size:125%;", 
                                                                      shiny::textOutput("fileExtensionWarning")))),
                        shiny::fluidRow(shiny::column(12, shiny::span(style = "color:red;font-size:125%;", 
                                                                      shiny::textOutput("colorWarning")))),
                        
                        # console output
                        shiny::pre(id = "console"),
                        
                        # Argument descriptions
                        shiny::h3("Below are some more details about each of the options on the left:"),
                        shiny::p(strong("data_dir : "),"	Absolute path to the folder containing your images"),
                        shiny::p(strong("model_type : "),"	Model types 'general', 'species', 'family', 'pig_only'.  
               The `general` model predicts mammals, birds, humans, and vehicles; latest version is V2.  
               The `species` model recognizes 75 species; latest version is V2. 
               The `family` model recognizes 31 families; latest version is V2. 
               The `pig_only` model recognizes only pigs; latest version is V1."),
                        shiny::p(strong("recurse : "),"	 TRUE/FALSE. Do you have images in subfolders within your data_dir that you want to analyze?"),
                        shiny::p(strong("file_extensions : " ),"	Types of images accepted by the model. Select all options represented in your data_dir."),
                        shiny::p(strong("make_plots : "),"	TRUE/FALSE. Do you want to make copies of your images with bounding boxes plotted on them?"),
                        shiny::p(strong("output_dir : "),"  Absolute path to output. Default is NULL; this creates a folder within your data_dir named after model type,
               date and time model was initiated. If resuming a previously saved checkpoint, you must populate this field to point to the folder with the checkpoint
               in question AND select the same model type/version."),
                        shiny::p(strong("sample50 : "),"  TRUE/FALSE. Do you want to run the model on a random sample of 50 images from your dataset? 
               This is a good idea if you are experimenting with settings. Note that a different random sample will generate for each model run."),
                        shiny::p(strong("write_bbox_csv"),"  TRUE/FALSE. Do you want to create a csv with all the information on predicted bounding boxes? 
               This csv will include all bounding boxes, even those with low probability."),
                        shiny::p(strong("score_threshold : "),"  Confidence threshold for returning a prediction and creating a bounding box. Accepts values from 0-0.99.
               A lower number will be less stringent, but may make more erroneous predictions. A higher number will be more stringent,
               but may miss correct predictions with confidence below the chosen threshold."),
                        shiny::p(strong("overlap_correction : "),"  TRUE/FALSE. Should overlapping detections be evaluated for proportion overlap (determined by overlap_threshold)
               and the highest confidence detection returned?"),
                        shiny::p(strong("overlap_threshold : "),"  Proportion of overlap for two detections to be considered a single detection. Accepts values from 0-0.99."),
                        shiny::p(strong("get_metadata : "),"  TRUE/FALSE. Collect image metadata as it runs through the model."),
                        shiny::p(strong("write_metadata : "),"  TRUE/FALSE. Write model predictions to image metadata as it runs through the model."),
                        shiny::p(strong("checkpoint_frequency : "),"  Save results after running a given number of images. Accepts values from 1-1000."),
                        shiny::p(strong("latitude and longitude : "),"  Optional image location to filter model predictions to species within range. Input takes one location per model run;
                     if images originate from multiple locations, separate them into different model runs based on location. Currently only applies to species model.")
                      )
                    )
    ),
    
    shiny::tabPanel("Visualize Results",
                    
                    ),
    shiny::tabPanel("Verify Results"))
  )
)
 