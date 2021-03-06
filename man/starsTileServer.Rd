% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/starsTileServer.R
\name{starsTileServer}
\alias{starsTileServer}
\title{A R6 class that extends plumber to function as a tile server}
\description{
Creates a tile server based on the R6 class \code{plumber::Plumber}.
In can be created both with a \code{stars} grid as well as a function or list of functions.
The main methods are \code{run} and \code{new}.
}
\examples{
m <- matrix(1:20, nrow = 5, ncol = 4)
dim(m) <- c(x = 5, y = 4) # named dim
(s <- stars::st_as_stars(m))
sf::st_crs(s) <- 4326
starsTileServer$new(s)
# Working directly from a file
grid <- system.file("tif/L7_ETMs.tif", package = "stars")
starsTileServer$new(grid)
\dontrun{
starsTileServer$new(s)$run()
}
}
\section{Super classes}{
\code{\link[plumber:Hookable]{plumber::Hookable}} -> \code{\link[plumber:Plumber]{plumber::Plumber}} -> \code{starsTileServer}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{starsTileServer$new()}}
\item \href{#method-add_tile_endpoint}{\code{starsTileServer$add_tile_endpoint()}}
\item \href{#method-get_grid}{\code{starsTileServer$get_grid()}}
\item \href{#method-get_attributes}{\code{starsTileServer$get_attributes()}}
\item \href{#method-get_dimensions}{\code{starsTileServer$get_dimensions()}}
\item \href{#method-get_dimension_values_chr}{\code{starsTileServer$get_dimension_values_chr()}}
\item \href{#method-get_non_spatial_dimensions}{\code{starsTileServer$get_non_spatial_dimensions()}}
\item \href{#method-clone}{\code{starsTileServer$clone()}}
}
}
\if{html}{
\out{<details ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="plumber" data-topic="Hookable" data-id="registerHooks">}\href{../../plumber/html/Hookable.html#method-registerHooks}{\code{plumber::Hookable$registerHooks()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="plumber" data-topic="Plumber" data-id="addAssets">}\href{../../plumber/html/Plumber.html#method-addAssets}{\code{plumber::Plumber$addAssets()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="plumber" data-topic="Plumber" data-id="addEndpoint">}\href{../../plumber/html/Plumber.html#method-addEndpoint}{\code{plumber::Plumber$addEndpoint()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="plumber" data-topic="Plumber" data-id="addFilter">}\href{../../plumber/html/Plumber.html#method-addFilter}{\code{plumber::Plumber$addFilter()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="plumber" data-topic="Plumber" data-id="addGlobalProcessor">}\href{../../plumber/html/Plumber.html#method-addGlobalProcessor}{\code{plumber::Plumber$addGlobalProcessor()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="plumber" data-topic="Plumber" data-id="call">}\href{../../plumber/html/Plumber.html#method-call}{\code{plumber::Plumber$call()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="plumber" data-topic="Plumber" data-id="filter">}\href{../../plumber/html/Plumber.html#method-filter}{\code{plumber::Plumber$filter()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="plumber" data-topic="Plumber" data-id="getApiSpec">}\href{../../plumber/html/Plumber.html#method-getApiSpec}{\code{plumber::Plumber$getApiSpec()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="plumber" data-topic="Plumber" data-id="getDebug">}\href{../../plumber/html/Plumber.html#method-getDebug}{\code{plumber::Plumber$getDebug()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="plumber" data-topic="Plumber" data-id="handle">}\href{../../plumber/html/Plumber.html#method-handle}{\code{plumber::Plumber$handle()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="plumber" data-topic="Plumber" data-id="mount">}\href{../../plumber/html/Plumber.html#method-mount}{\code{plumber::Plumber$mount()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="plumber" data-topic="Plumber" data-id="onHeaders">}\href{../../plumber/html/Plumber.html#method-onHeaders}{\code{plumber::Plumber$onHeaders()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="plumber" data-topic="Plumber" data-id="onWSOpen">}\href{../../plumber/html/Plumber.html#method-onWSOpen}{\code{plumber::Plumber$onWSOpen()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="plumber" data-topic="Plumber" data-id="openAPIFile">}\href{../../plumber/html/Plumber.html#method-openAPIFile}{\code{plumber::Plumber$openAPIFile()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="plumber" data-topic="Plumber" data-id="print">}\href{../../plumber/html/Plumber.html#method-print}{\code{plumber::Plumber$print()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="plumber" data-topic="Plumber" data-id="registerHook">}\href{../../plumber/html/Plumber.html#method-registerHook}{\code{plumber::Plumber$registerHook()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="plumber" data-topic="Plumber" data-id="removeHandle">}\href{../../plumber/html/Plumber.html#method-removeHandle}{\code{plumber::Plumber$removeHandle()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="plumber" data-topic="Plumber" data-id="route">}\href{../../plumber/html/Plumber.html#method-route}{\code{plumber::Plumber$route()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="plumber" data-topic="Plumber" data-id="run">}\href{../../plumber/html/Plumber.html#method-run}{\code{plumber::Plumber$run()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="plumber" data-topic="Plumber" data-id="serve">}\href{../../plumber/html/Plumber.html#method-serve}{\code{plumber::Plumber$serve()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="plumber" data-topic="Plumber" data-id="set404Handler">}\href{../../plumber/html/Plumber.html#method-set404Handler}{\code{plumber::Plumber$set404Handler()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="plumber" data-topic="Plumber" data-id="setApiSpec">}\href{../../plumber/html/Plumber.html#method-setApiSpec}{\code{plumber::Plumber$setApiSpec()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="plumber" data-topic="Plumber" data-id="setDebug">}\href{../../plumber/html/Plumber.html#method-setDebug}{\code{plumber::Plumber$setDebug()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="plumber" data-topic="Plumber" data-id="setDocs">}\href{../../plumber/html/Plumber.html#method-setDocs}{\code{plumber::Plumber$setDocs()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="plumber" data-topic="Plumber" data-id="setDocsCallback">}\href{../../plumber/html/Plumber.html#method-setDocsCallback}{\code{plumber::Plumber$setDocsCallback()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="plumber" data-topic="Plumber" data-id="setErrorHandler">}\href{../../plumber/html/Plumber.html#method-setErrorHandler}{\code{plumber::Plumber$setErrorHandler()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="plumber" data-topic="Plumber" data-id="setParsers">}\href{../../plumber/html/Plumber.html#method-setParsers}{\code{plumber::Plumber$setParsers()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="plumber" data-topic="Plumber" data-id="setSerializer">}\href{../../plumber/html/Plumber.html#method-setSerializer}{\code{plumber::Plumber$setSerializer()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="plumber" data-topic="Plumber" data-id="swaggerFile">}\href{../../plumber/html/Plumber.html#method-swaggerFile}{\code{plumber::Plumber$swaggerFile()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="plumber" data-topic="Plumber" data-id="unmount">}\href{../../plumber/html/Plumber.html#method-unmount}{\code{plumber::Plumber$unmount()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
This method is used to initialize a new tile server
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{starsTileServer$new(grid, colorFun = NULL, tileSize = 256, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{grid}}{Either a stars grid, a path pointing towards a gridded file, a function or a list of named functions}

\item{\code{colorFun}}{a color function to use for coloring the map tiles, the function needs to be the same format as \code{\link[leaflet:colorNumeric]{leaflet::colorNumeric()}}. It is important to specify a color function as it is important to keep the range of the color scale similar between tiles, therefore the minimum and maximum needs to be fixed. It can also be a list of color functions.}

\item{\code{tileSize}}{The size of the tile (generally 256 pixels, and not tested with other sizes)}

\item{\code{...}}{Arguments passed on to the \link[plumber:Plumber]{plumber::Plumber}, most important is the \code{port} number.}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
If \code{grid} is a function it should take a stars grid as the first argument and return the grid with the same topology with values attached.
Any other arguments to the function will be part of the API and will be passed to the function as characters.
}

\subsection{Returns}{
An \code{starsTileServer} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-add_tile_endpoint"></a>}}
\if{latex}{\out{\hypertarget{method-add_tile_endpoint}{}}}
\subsection{Method \code{add_tile_endpoint()}}{
Add three endpoints to the tile server, to return both the tiles and the color scale used.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{starsTileServer$add_tile_endpoint(prefix, handlerFun, colorFun, params)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{prefix}}{the name to be used by the server for this tile server}

\item{\code{handlerFun}}{The function that handles the api request and returns the grid}

\item{\code{colorFun}}{The color function to use for example \code{\link[leaflet:colorNumeric]{leaflet::colorNumeric()}}}

\item{\code{params}}{parameters passed on to the \code{new} method of \link[plumber:PlumberEndpoint]{plumber::PlumberEndpoint}}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_grid"></a>}}
\if{latex}{\out{\hypertarget{method-get_grid}{}}}
\subsection{Method \code{get_grid()}}{
return the grid used to initialize the function
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{starsTileServer$get_grid()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_attributes"></a>}}
\if{latex}{\out{\hypertarget{method-get_attributes}{}}}
\subsection{Method \code{get_attributes()}}{
return the attributes of the stars grid
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{starsTileServer$get_attributes()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_dimensions"></a>}}
\if{latex}{\out{\hypertarget{method-get_dimensions}{}}}
\subsection{Method \code{get_dimensions()}}{
return the names of the dimensions of the grid
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{starsTileServer$get_dimensions()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_dimension_values_chr"></a>}}
\if{latex}{\out{\hypertarget{method-get_dimension_values_chr}{}}}
\subsection{Method \code{get_dimension_values_chr()}}{
return the values of a dimension as a character
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{starsTileServer$get_dimension_values_chr(x)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{x}}{the name of the dimension}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_non_spatial_dimensions"></a>}}
\if{latex}{\out{\hypertarget{method-get_non_spatial_dimensions}{}}}
\subsection{Method \code{get_non_spatial_dimensions()}}{
return all non spatial dimensions
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{starsTileServer$get_non_spatial_dimensions()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{starsTileServer$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
