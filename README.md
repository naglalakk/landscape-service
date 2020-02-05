donnabot-service
===

API to use with donnabot.dev frontend

### Installation

This service runs on Haskell which requires you to install the Haskell platform as well as Cabal, Haskell's package manager. The easiest way to achieve this is through ghcup.
Check out installation instructions [here](https://github.com/haskell/ghcup#installation)

This project uses Cabal version > 3.0. Once Cabal is installed verify the version with the following command

    cabal --version

If your version is less than 3.0 then you need to update Cabal with the following command

    cabal install cabal-install-3.0.0.0

### Running the API 

To run the api start by building it to get the rfp-service executable

    cabal new-build --reorder-goals

Then run it with the following command

    cabal new-exec donnabot-service --reorder-goals


### THINGS TO NOTE

The static path is static/uploads. This folder is omitted
from the repository since we don't want to store arbitrary
static files within the repo. When first running this project
you must create this folder if you want the upload to work properly. At the root of this project simply do

    mkdir static/uploads

Additionally you need to create a folder for the log files, used in production

    mkdir logs

### Env variables

Environment variables can be stored at the root of this project in a .env file. The current env variables used by this service are the following

**ENV**

Current environment. The options are

- Development
- Production

Switching these environments mostly changes how logging is managed

**PORTNR**

The port number for the service to run on. 

**PGHOST**

Postgres host (e.g. localhost)

**PGPORT**

Postgres port

**PGUSER**

Postgres username

**PGPASS**

Postgres password

**PGDATABASE**

Postgres database
