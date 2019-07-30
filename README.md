# Akka_study

## Description

This project aim to make a study on how the users of the akka framework use it in
scala by seeing how these users create actors, if they use persistant actor or
atLeastOnceDelivery, if they import scalaTest or akkaTestKit and principaly how
deep their actor hierarchy is.

This project use the [ScrapyScrap repository](https://github.com/CuadrosNicolas/ScrapyScrap)
for scraping github.

Results may vary between execution since repositories from github can be updated between
reproductions.


## How to use

First you need to initialize the scraping project, for this see the README.md
file under the scraping folder.

Next you need to make the reproduction script executable :

```bash
chmod +x ./reproduce.sh
```

Then you can simply run the reproduction script :

```bash
./reproduce.sh
```

The reproduction can take 1 or 2 days just for getting akka repositories and
10 minutes for the analysis part. So you should run the reproduction on a server.

## Analyse the results

The results of the analysis will be store in the akka_process folder under the name
Results_projects_actors as a csv file.

You can import this file to Google Sheet or Microsoft Excel.

Project that use special actor spawn such as anonymous class will be designed as fail
since you cannot apply depth analysis on them, so you can directly remove these from the results.