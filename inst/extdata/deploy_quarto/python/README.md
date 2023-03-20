```bash
# Create a virtual environment
python -m venv .venv
source .venv/bin/activate

# Install flask
pip3 install jupyter numpy matplotlib
quarto preview index.qmd
pip3 freeze > requirements.txt
rsconnect deploy quarto --server https://rsc.jumpingrivers.cloud/ --api-key .

deactivate
#rsconnect details --server https://rsc.jumpingrivers.cloud/
```
