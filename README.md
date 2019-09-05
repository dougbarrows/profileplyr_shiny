# profileplyr_shiny
A shiny app for profileplyr, a Bioconductor package (including Docker image)


## How to use

To use this app, either clone this repository or use the Docker image

### Clone Repository and run app locally

1.  Click the 'Clone or Download' button on this page, and download the files (e.g. by then pressing the 'Download ZIP' button) associated with this shiny app. 
2.  Open an R Studio workspace. NOTE: Contained within the files that are downloaded is an R Studio project titled, 'profileplyr_shiny.Rproj'. This can be a convenient place from which to run the app, however the app can be opened and run from any R Studio window. 
3.  Open the 'app.R' R script in the 'cerulean_theme/app' directory.
4.  Click the 'Run app' button on the top right corner of the R Studio editor. The app should open in the browser, but you can hit the dropdown next to this button and change the settings so the app opens in a separate window.


### Using the Docker image

By copying the Docker image (hosted [here](https://cloud.docker.com/repository/docker/dougbarrows/my-shiny-app/general) on Docker Hub), the proper versions of profileplyr, R, and the dependencies will be loaded within the image. This should be the easiest method to make the app run as it is supposed to. 

1.  Install Docker for [Mac](https://docs.docker.com/docker-for-mac/install/) or [Windows](https://docs.docker.com/docker-for-windows/install/)
2.  Enter the following command into the terminal to get the image from Docker Hub. This will pull the image if you don't already have it, and will run a container on your computer. If you have already pulled the image from Docker Hub then this same line of code will be used to run the container.

    ```
    $ docker run --rm -p  3838:3838 dougbarrows/my-shiny-app:latest
    ```
  
3.  Open an internet browser and go to 'http://localhost:3838' and the Shiny app should appear in the browser. You should notice that the R messages/warnings/errors appear in the terminal as the container is being run.  



   