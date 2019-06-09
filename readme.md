I wrote this tool for my father, Antonio Cussen, who was writing "El
milenio según Virgilio," or "Virgil's Millennium," an essay about
Virgil's Aeneid.  He wanted to reconstruct the epic poem as it was
originally written by the Roman poet Virgil for Emperor Augustus.
Part of the task was to identify how the letters were to be counted,
because the number of letters in the poem is meaningful.

Written between 29 and 19 BCE, only two decades after the of the
Julian reform of the calendar, the Aeneid immortalizes that calendar
in a poem.  At the time, there was a short Alexandrian poem of 365
letters, one per day of the year, which literally says it has a letter
for each day of the year.  The Aeneid similarly has a letter per day
in a Great Year.  A Great Year, as Virgil conceives of it, is a period
of 9 saecula (centuries) of 110 years each, culminating in a ten year
period of destruction from which all things emerge renewed.  The
beginning of the Aeneid covers the Trojan War, which was the period of
destruction after which began the Great Year of the Roman Republic.
The commentary is that this Great Year was ending with the period of
destruction of Augustus's brutal reign, which marked the beginnig of
the new era of the Empire.

Included is an Aeneid as published in Volume II of "Virgil's
Millennium."  It is in Latin and its correctness has been approximated
to the highest degree made possible by surviving manuscripts and the
hidden mathematical structure of the poem itself.  It's choices are
defended in "Notas para la reconstrucción de la Eneida" ("Notes for
the reconstruction of the Aeneid"), Volume III of the three tomes of
Cussen's work.  This includes research into the orthography of Latin
words of the time.

Once a verse has been preprocessed according to these rules about
which letters are not pronounced, the poem's letters can be tallied.
This is what virgil.lisp does.

This cryptographic algorithm was performed in the 20th century by US
citizens, and presumably in the 1st century by Virgil's Greek slaves.
Needless to say it is hugely advantageous to do so with a computer in
the 21st century, and with the poem and the code online, anybody can
verify it one verse at a time:

* Usage

Load a Common Lisp, e.g. clisp, and run

> clisp script.lisp aeneid.txt

Alternatively, to make a file to compare to elided-aeneid.txt

> clisp script.lisp aeneid.txt > output.txt

* With SBCL from SLIME

> (load "virgil.lisp")
> (process "aeneid.txt")

* Notes

The code annotates the plain aeneid in several ways.  First, it adds
verse numbers from 1 to 9900, which are even more important in this
poem than they already are in most poetry.  Second, is the cumulative
letter count.  This helped with reconstructing the poem, especially by
finding differences between the output of different versions of the
aeneid.  Finally there's the verse itself, with elided letters in
parentheses.