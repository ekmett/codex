smawk
=====

[![Hackage][hackage-shield]][hackage]

This package implements the SMAWK algorithm by
[Peter Shor, Shlomo Moran, Alok Aggarwal, Robert Wilber and Maria Klawe][smawk]
for finding the minimum value in each row of an implicitly defined totally monotone matrix.

This has many applications in computational geometry, such as finding the farthest point
from each point in a convex polygon, finding optimal enclosing polygon. It can also be
used to implement paragraph line breaking in a manner analogous to Knuth and Platt, but in
linear time. It also has uses in RNA secondary structure prediction, various sequence
alignment problems, construction of prefix codes, image thresholding, etc.

Contact Information
-------------------

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the #haskell IRC channel on irc.freenode.net.

-Edward Kmett

[smawk]: https://link.springer.com/article/10.1007%2FBF01840359
[hackage]: https://hackage.haskell.org/package/smawk
[hackage-shield]: https://img.shields.io/hackage/v/smawk.svg
