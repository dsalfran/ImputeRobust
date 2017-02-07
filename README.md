ImputeRobust: Multiple Imputation with GAMLSS
=============================================

The `ImputeRobust` package adds to [https://cran.r-project.org/web/packages/mice/index.html](MICE) an imputation method based on generalized additive models for location, scale, and shape introduced by de Jong (2012), de Jong, van Buuren and Spiess (2016). It has been tested mostly with continous variables, count, and binary data.

Installation
------------

The latest version can be installed from GitHub as follows:

``` r
install.packages("devtools")
devtools::install_github(repo = "dsalfran/ImputeRobust")
```

Main functions
--------------

The main functions in the `ImputeRobust` package are:

<table>
<colgroup>
<col width="26%" />
<col width="73%" />
</colgroup>
<thead>
<tr class="header">
<th>Function Name</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>mice.impute.gamlss()</code></td>
<td>Impute data using GAMLSS</td>
</tr>
<tr class="even">
<td><code>mice.impute.gamlssBI()</code></td>
<td>Same as <code>mice.impute.gamlss()</code> but specially tailored for binary data</td>
</tr>
</tbody>
</table>

References
----------

de Jong, R., van Buuren, S. & Spiess, M. (2016) Multiple Imputation of Predictor Variables Using Generalized Additive Models. Communications in Statistics -- Simulation and Computation, 45(3), 968--985.

de Jong, Roel. (2012). “Robust Multiple Imputation.” Universität Hamburg. .

Rigby, R. A., and Stasinopoulos, D. M. (2005). Generalized Additive Models for Location, Scale and Shape. Journal of the Royal Statistical Society: Series C (Applied Statistics) 54 (3): 507–54.
