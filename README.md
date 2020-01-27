lc-paper

To use `minted` one needs to install `pygments`

```
sudo apt-get install python-pygments
```

and add `--shell-escape` flag for `pdflatex`.

For TeXstudio:

Options > Configure TeXstudio > Commands > PdfLaTeX
```
pdflatex -synctex=1 -interaction=nonstopmode --shell-escape %.tex
```
