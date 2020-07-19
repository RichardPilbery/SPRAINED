# SPRAINED study
The Specialist Paramedic Rotations And Their Impact on Non-conveyancE Decisions (SPRAINED): a controlled interrupted time series analysis

## What is the SPRAINED study?
The National Health Service (NHS) in the United Kingdom is facing a 5% increase in demand every year for urgent and emergency care services, and there is evidence that patients are being taken to hospitals by ambulance services when they do not need to go. This is a problem because emergency departments are becoming more crowded, which can lead to poorer quality care. Also, less ambulances are available to respond to emergencies, because they are queueing at hospital for a long time.

To improve the care Yorkshire Ambulance Service provide to their patients, some paramedics have received additional training. These advanced paramedics have been very successful at treating patients in their own home safely. However, their training is long and expensive, so another role, the specialist paramedic role has been introduced. Their training does not take as long and is cheaper to provide. However, the specialist paramedics do not appear to keep patients safely at home more often than regular paramedics. Recently, the specialist paramedics have taken part in a national paramedic programme, where they are given the chance to work in GP surgeries and emergency call centres.

This study aims to see if specialist paramedics who have worked in a GP surgery for 10 weeks, can keep patients at home safely, and without costing too much, more often than regular paramedics.

## Can I replicate the data analysis?
Yes, mostly! I have created a docker image which will enable you to recreate the environment I used to undertake the analysis, as well as a GitHub repo that contains the code and a synthetic dataset. Unfortunately, permission was not obtained to share the real data, so I've created a synthetic dataset which is similar to the original data. Just follow these steps:

1. Install [Docker](https://www.docker.com/products/docker-desktop)
2. Run Docker
3. Copy the [Dockerfile](https://raw.githubusercontent.com/RichardPilbery/SPRAINED/master/Dockerfile) and place it into an empty folder
3. Open a terminal window (or Powershell in Windoze) and change the directory to match the folder that contains the Dockerfile
4. Enter the following command: `docker build ./` WARNING: The docker image is around 3.3Gb in size.
5. This might take a while, but you should end with a comment like *Successfully built IMAGEID* ![Build success](https://github.com/RichardPilbery/SPRAINED/raw/master/images/01-build.png)
6. Run the command: `docker run --rm -p 127.0.0.1:8787:8787 -e DISABLE_AUTH=true IMAGEID` where IMAGEID is the IMAGEID value from step 6. Note that this command disables authentication, which is fine for a local version, but not if you plan on hosting it somewhere...
7. Open a browser window and head to: *http://127.0.0.1:8787*.
8. In the browser window, you should see an RStudio environment. Click on the *SPRAINED-master* folder to open it. ![RStudio environment](https://github.com/RichardPilbery/SPRAINED/raw/master/images/02-rstudio.png)
9. Scroll down to find the project file, called *SPRAINED.Rproj* and click it. ![Open project](https://github.com/RichardPilbery/SPRAINED/raw/master/images/03-run-proj.png)
10. Confirm that you'd like to open the project by clicking *Yes* ![Open project](https://github.com/RichardPilbery/SPRAINED/raw/master/images/04-confirm.png)
11. In the top-right hand window, click on the Build tab and then by clicking on the triangle next to the Build Book button, choose the *bookdown::pdf_document2* option. ![Build book](https://github.com/RichardPilbery/SPRAINED/raw/master/images/05-build.png)
12. Be patient, this might take 5--10 minutes, but once completed a new window with the rendered PDF should appear.

## Funding
This paper presents independent research by the NIHR Applied Research Collaboration Yorkshire and Humber (ARC YH). The views expressed in this publication are those of the author(s) and not necessarily those of the National Institute for Health Research or the Department of Health and Social Care.

## Trial registration
ClinicalTrials.gov identifier: [NCT04193800](https://clinicaltrials.gov/ct2/show/NCT04193800)

## Acknowledgements
This work uses data provided by patients and collected by the NHS as part of their care and support. The authors would also like to thank the Yorkshire Ambulance service business intelligence team who collated the data used in this study.
