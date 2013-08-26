#!/bin/zsh

echo "Generating pyg.tex..."
pygmentize -S default -f latex > pyg.tex

echo "Generating TeX files for the individual listings..."
for clj in lst/*.clj; do
    echo "Pygmentizing ${clj}..."
    pygmentize \
	-f latex \
	-P "verboptions=fontsize=\\footnotesize" \
	-P "style=default" \
	-o ${clj/clj/tex} ${clj}
done

echo done
