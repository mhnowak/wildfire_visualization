# wildfire_visualization

## Installation

### Install R

#### MacOS: 

```bash
brew install r
```

#### Windows:

TODO

#### Linux:
```bash
sudo apt install -y r-base r-base-dev
```
Add library:

```bash
sudo apt install -y software-properties-common dirmngr gnupg
wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc \
| sudo tee /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
sudo add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu jammy-cran40/"
sudo apt update
sudo apt install -y r-base
```

To install additional libraries/packages:

example: DAAG
```bash
R
install.packages("DAAG")
library(DAAG)
q()
```

### IDE

You can install R studio or use VSCode with R extension.

You can use JupyterNotebook for R by installing additional libraries.

Linux:
```bash
pip install jupyterlab notebook

R
install.packages("IRkernel")
IRkernel::installspec(user = FALSE)
IRkernel::installspec(user = TRUE)
q()

jupyter kernelspec list

```

### Download data

1. Download FPA_FOD_20170508.sqlite file from [kaggle](https://www.kaggle.com/datasets/rtatman/188-million-us-wildfires/data)
2. Put it inside this folder

## Useful tools

1. [DB Browser for SQLite](https://sqlitebrowser.org/) - browse the data from SQLite quickly
2. [Air - R language formatter](https://open-vsx.org/vscode/item?itemName=posit.air-vscode) - VS Code extension