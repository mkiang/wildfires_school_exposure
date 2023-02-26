## Download and unzip data files ----
## If you prefer to download the data files yourself, just go to: 
## https://osf.io/f4aku/ and decompress all files into the project root. 

if (!file.exists("./data_temp.zip")) {
    download.file("https://osf.io/f4aku/download", 
                  destfile = "./data_temp.zip")
}

if (file.exists("./data_temp.zip")) {
    unzip(zipfile = "./data_temp.zip", exdir = "./")
    file.remove("./data_temp.zip")
}
