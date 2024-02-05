| NOTE        |
|:---------------------------|
| Work in progress: English translation has been generated locally from French with [libretranslate](https://github.com/LibreTranslate/LibreTranslate). It is far from correct, and for testing purposes only. |

Internationalization (also i18n) is the process of easing the use of different languages in a software <sup>[1](https://journal.r-project.org/articles/RN-2005-001/RN-2005-001.pdf)</sup>.

To facilitate switching language in `easieR`, dictionnary files have been created in the `./R` directory of this package.
They are named according to the language they correspond to (e.g. `fr_FR`), and contain *almost* every strings visible to users.
Those files can be copied and translated into another language "easily".
Each of the strings they contain is associated to a variable's name (its specific identifier).
Those variables act like placeholders in easieR code, pointing to their matching string.
Files in this folder can therefore be seen as mutable dictionnaries.
Any number of dictionnaries can be created for different languages.

# Placeholders

To avoid collision with easieR or other packages variables, placeholders must have unique variables names (their identifiers).  
For now, there are three types of placeholders, all starting with a specific pattern :

- Interactive : start with `ask_`. They are asking the user to do something.
- Informative : start with `desc_`. They describe/indicate something (titles, windows names, how $x$ works, etc.)
- Uncategorized : start with `txt_`. TODO.

Those categories are not well delimited, but it will be quite easy to modify them in the future.

⚠ [I](https://github.com/dougy147) bare full responsibility for poorly named variables. 
While strings translation was done with libretranslate, variables name were written by hand... casually.
They therefore contain bad anglicisms and errors, but I hope changing them now to be less costly than redesigning the language system itself.

# Hard coded strings

Some strings are better to be hard coded in easieR scripts, for example packages names (`BayesFactor`, `outliers`,...).
To avoid mistakes during translation, I surrounded those strings with single quotes (`'`) instead of double quotes (`"`).
Double quotes are now reserved for placeholders (i.e. replacable strings) (to be done).

# For programmers

Translation process might have been a bit agressive, with some invisible-to-the-user strings uselessly replaced by placeholders. 
While not affecting easieR proper operation, it has made the code less easy to read.

To continue coding on easieR *with* strings displayed in place of placeholders, one script will help switching between strings and placeholders (TODO).

# Did it break `easieR`?

Apparently no. But testing required!

Same analysis on same datasets gave exact same results for:

- descriptive statistics
- chi-squared (fit, independance, McNemar)
- correlations (detailed analysis, matrix correlation (except partial without outliers), two correlations)
- t.tests (norm, paired, independant (except with bootstraps+bayesian))
- ANOVA (independant, repeated, mixt)
- regressions (classic)
- factor component analysis (partial testing on exploratory)

Not tested :

- correlations > matrix partial correlations removing outliers : outputs "unused argument (X = c(X, Y, Z))" in VI.multiples
- correlations > other correlations
- t.test > independant > full analysis (robusts+bayesian not working) 
- ancova
- regressions (logistics, mediation effect)
- factor component analysis
- reliability and agreement

# Limitations

1) `switch` cannot match `EXPR` to variables, even if they point to a string. That's why I replaced every `switch` by `if..else` conditions.

2) The "dictionnaries" method loads a lot of objects in memory. This is potentially annoying. Placeholders variables are stored in accessible variables. Moreove those variables are made global (might not be good practise).

# Done

- Language can be dynamically selected from GUI (`Interface` > `Choose language`) or command-line (e.g. `load_language("English")`)
- Selected languages displays in reports as well as command-line

# TODO

- Chose better variables names for placeholders
- Make placeholders variables less visible to the user environment
- Data interpolation system ("placeholders for data" inside placeholders)
