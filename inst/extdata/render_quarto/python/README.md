`requirements.txt` lists only what `index.qmd` needs, with lower bounds rather than
exact pins, so pip picks versions that suit the Python on the server. Don't replace it
with `pip freeze`: a frozen numpy can't install on both Python 3.10 and 3.14.

```bash
# Create a virtual environment
python -m venv .venv
source .venv/bin/activate

# Install the packages in requirements.txt
pip3 install -r requirements.txt
quarto preview index.qmd
rsconnect deploy quarto --server https://rsc.jumpingrivers.cloud/ --api-key .

deactivate
#rsconnect details --server https://rsc.jumpingrivers.cloud/
```
