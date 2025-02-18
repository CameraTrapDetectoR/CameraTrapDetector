# CameraTrapDetectoR: Detect, classify, and count animals in camera trap images  

Use deep learning computer vision models to automatically detect, count, and classify common North American domestic and wild species in camera trap images. The models can be deployed R function or in an interactive Shiny application on a local machine with little to no coding needed. CameraTrapDetectoR makes it easy to harness the power of AI image processing while protecting dataset privacy, and integrates seamlessly into existing research workflows.

\
*Note: this package takes over development of previous github repository 'TabakM/CameraTrapDetectoR'. If you have downloaded the previous version of CameraTrapDetectoR, please uninstall and reinstall from this repository to stay connected with ongoing package developments.*  


## About the Models

CameraTrapDetectoR models are now compatible only with package version $>=$  v1.0.0  Please reinstall the package if your version is older.

Four types of models are available: 
  1. a taxonomic **class** model that classifies objects as mammal or avian
  2. a taxonomic **family** model that recognizes mammal, avian, and reptile families
  3. a taxonomic **species** model that recognizes unique domestic and wild species including all North American wild cat species, bear species, and Canid species.

The taxonomic models also include categories for vehicles and humans. All models include a category for empty images.  

New model versions are trained iteratively over time. All finalized and verified models are available in the latest package version and can be specified by the user (e.g. "species_v1", "species_v2"). Each model + version is independently trained; different models can be run sequentially and treated as independent observers. Specifying only the model type (e.g. "species" or "family") will default to the latest model generation. The latest version for the species model is currently **version 3**; the latest version for the family and general models is **version 2**.  
  


## Install CameraTrapDetectoR


Install devtools if you don't have it, then install CameraTrapDetectoR:  

```
if (!require('devtools')) install.packages('devtools')  
devtools::install_github("CameraTrapDetectoR/CameraTrapDetectoR")
```
Agree to update all necessary packages. 

See the [Installation Guide](https://github.com/CameraTrapDetectoR/CameraTrapDetectoR/wiki/Installation) for more details, including instructions to install from source.  
  

## Load the Package
```
library(CameraTrapDetectoR)
```

When you first download the package, you will also need to accept downloads from ml-verse packages. It may be easier to load these packages directly the first time you use CameraTrapDetectoR and affirm the additional software downloads.

```
library(torch)
library(torchvision)
library(torchvisionlib)
```


## Deploy the Model in Console  

Deploy the model from the console with `deploy_model`. Here is a brief example:  
  
```
# specify the path to your images
data_dir = "C:/Users/user.name/path/to/image/directory" 

# deploy the model and store the output dataframe as predictions
predictions <- deploy_model(data_dir = data_dir, # this argument is the only required input; all other options have defaults that should be examined for best results
                            model_type = "general", # this argument runs the general class model. Default is "species"
                            recursive = FALSE, # disable recursive searching through sub-folders of data_dir
                            sample50 = TRUE) # this will cause the model to only work on 50 random images in your dataset. Default is FALSE
```
There are many more options for this function. To insure the arguments are set to your specifications, type `?deploy_model` in your R console, or consult the [wiki](https://github.com/CameraTrapDetectoR/CameraTrapDetectoR/wiki/Function-Arguments) for details. 


The package downloads weights and model architecture of the large neural network; if you are on a slow internet connection, you may need to modify your options. By default, R will timeout downloads at 60 seconds. Running the line below will increase the operation timeout (units are in seconds). Feel free to use a larger number than 200 if you are on a very slow connection.

```
options(timeout=200)
```  

You may also need to disconnect from VPN while downloading model weights.  If this solution still doesn't work, you may want to explore the [command line option](insert link to cl docs) to run the models. 

## Deploy the Model in Shiny
Copy + paste this code to the R console to launch the interactive app:
```
runShiny("deploy")
```
This will launch a Shiny App on your computer. See the **Shiny Demo** vignette or the [wiki](https://github.com/CameraTrapDetectoR/CameraTrapDetectoR/wiki/Shiny-Tutorial) for a complete example on using the Shiny app. 

## Save your work  
CameraTrapDetectoR allows you saves your results at a user-chosen frequency with the **checkpoint_frequency** argument of the **deploy_model** function; if something happens to your machine during the model run, the model's work will be preserved. To resume a previously-initiated model run, specify the folder where your results are stored as your **output_dir** argument. CameraTrapDetectoR will read in these results and run the model only on images that do not have existing results. To avoid errors, do not modify the contents of the results files or your **deploy_model** function arguments.


## Citation

Tabak, M. A., Falbel, D., Hamzeh, T., Brook, R. K., Goolsby, J. A., Zoromski, L. D., Boughton, R. K., Snow, N. P., VerCauteren, K. C., & Miller, R. S. (2022). CameraTrapDetectoR: Automatically detect, classify, and count animals in camera trap images using artificial intelligence (p. 2022.02.07.479461). bioRxiv. [link to manuscript](https://doi.org/10.1101/2022.02.07.479461)

Or\
@article {Tabak2022.02.07.479461,
	author = {Tabak, Michael A and Falbel, Daniel and Hamzeh, Tess and Brook, Ryan K and Goolsby, John A and Zoromski, Lisa D and Boughton, Raoul K and Snow, Nathan P and VerCauteren, Kurt C and Miller, Ryan S},
	title = {CameraTrapDetectoR: Automatically detect, classify, and count animals in camera trap images using artificial intelligence},
	elocation-id = {2022.02.07.479461},
	year = {2022},
	doi = {10.1101/2022.02.07.479461},
	publisher = {Cold Spring Harbor Laboratory},,
	URL = {https://www.biorxiv.org/content/10.1101/2022.02.07.479461v1},
	eprint = {https://www.biorxiv.org/content/10.1101/2022.02.07.479461v1.full.pdf},
	journal = {bioRxiv}
}
