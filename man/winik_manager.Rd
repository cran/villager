% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/winik_manager.R
\docType{class}
\name{winik_manager}
\alias{winik_manager}
\title{Winik Manager}
\description{
A class that abstracts the management of aggregations of Winik classes. Each village should have
an instance of a winik_manager to interface the winiks inside.
}
\section{Methods}{

\describe{
\item{\code{add_winik()}}{Adds a single winik to the manager.}
\item{\code{get_average_age()}}{Returns the average age, in years, of all the winiks.}
\item{\code{get_living_winiks()}}{Gets a list of all the winiks that are currently alive.}
\item{\code{get_states()}}{Returns a data.frame consisting of all of the managed winiks.}
\item{\code{get_winik()}}{Retrieves a particular winik from the manager.}
\item{\code{get_winik_index()}}{Retrieves the index of a winik.}
\item{\code{initialize()}}{Creates a new manager instance.}
\item{\code{load()}}{Loads a csv file defining a population of winiks and places them in the manager.}
\item{\code{remove_winik()}}{Removes a winik from the manager}
}
Creates a new winik manager instance.
}

\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{winiks}}{A list of winiks objects that the winik manager manages.}

\item{\code{winik_class}}{A class describing winiks. This is usually the default villager supplied 'winik' class}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{winik_manager$new()}}
\item \href{#method-get_winik}{\code{winik_manager$get_winik()}}
\item \href{#method-get_living_winiks}{\code{winik_manager$get_living_winiks()}}
\item \href{#method-add_winik}{\code{winik_manager$add_winik()}}
\item \href{#method-remove_winik}{\code{winik_manager$remove_winik()}}
\item \href{#method-get_states}{\code{winik_manager$get_states()}}
\item \href{#method-get_winik_index}{\code{winik_manager$get_winik_index()}}
\item \href{#method-connect_winiks}{\code{winik_manager$connect_winiks()}}
\item \href{#method-get_living_population}{\code{winik_manager$get_living_population()}}
\item \href{#method-get_average_age}{\code{winik_manager$get_average_age()}}
\item \href{#method-add_children}{\code{winik_manager$add_children()}}
\item \href{#method-load}{\code{winik_manager$load()}}
\item \href{#method-clone}{\code{winik_manager$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{winik_manager$new(winik_class = villager::winik)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{winik_class}}{The class that's being used to represent agents being managed
Given the identifier of a winik, sort through all of the managed winiks and return it
if it exists.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_winik"></a>}}
\if{latex}{\out{\hypertarget{method-get_winik}{}}}
\subsection{Method \code{get_winik()}}{
Return the R6 instance of a winik with identiifier 'winik_identifier'.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{winik_manager$get_winik(winik_identifier)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{winik_identifier}}{The identifier of the requested winik.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
An R6 winik object
Returns a list of all the winiks that are currently alive.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_living_winiks"></a>}}
\if{latex}{\out{\hypertarget{method-get_living_winiks}{}}}
\subsection{Method \code{get_living_winiks()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{winik_manager$get_living_winiks()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A list of living winiks
Adds a winik to the manager.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-add_winik"></a>}}
\if{latex}{\out{\hypertarget{method-add_winik}{}}}
\subsection{Method \code{add_winik()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{winik_manager$add_winik(new_winik)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{new_winik}}{The winik to add to the manager}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
None
Removes a winik from the manager
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-remove_winik"></a>}}
\if{latex}{\out{\hypertarget{method-remove_winik}{}}}
\subsection{Method \code{remove_winik()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{winik_manager$remove_winik(winik_identifier)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{winik_identifier}}{The identifier of the winik being removed}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
None
Returns a data.frame of winiks
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_states"></a>}}
\if{latex}{\out{\hypertarget{method-get_states}{}}}
\subsection{Method \code{get_states()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{winik_manager$get_states()}\if{html}{\out{</div>}}
}

\subsection{Details}{
Each row of the data.frame represents a winik object
}

\subsection{Returns}{
A single data.frame of all winiks
Returns the index of a winik in the internal winik list
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_winik_index"></a>}}
\if{latex}{\out{\hypertarget{method-get_winik_index}{}}}
\subsection{Method \code{get_winik_index()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{winik_manager$get_winik_index(winik_identifier)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{winik_identifier}}{The identifier of the winik being located}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The index in the list, or R's default return value
Connects two winiks together as mates
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-connect_winiks"></a>}}
\if{latex}{\out{\hypertarget{method-connect_winiks}{}}}
\subsection{Method \code{connect_winiks()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{winik_manager$connect_winiks(winik_a, winik_b)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{winik_a}}{A winik that will be connected to winik_b}

\item{\code{winik_b}}{A winik that will be connected to winik_a
Returns the total number of winiks that are alive}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_living_population"></a>}}
\if{latex}{\out{\hypertarget{method-get_living_population}{}}}
\subsection{Method \code{get_living_population()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{winik_manager$get_living_population()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
The numnber of living winiks
Returns the averag age, in years, of all of the winiks
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_average_age"></a>}}
\if{latex}{\out{\hypertarget{method-get_average_age}{}}}
\subsection{Method \code{get_average_age()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{winik_manager$get_average_age()}\if{html}{\out{</div>}}
}

\subsection{Details}{
This is an \emph{example} of the kind of logic that the manager might handle. In this case,
the manager is performing calculations about its aggregation (winiks). Note that the 364 days needs to
work better
}

\subsection{Returns}{
The average age in years
Takes all of the winiks in the manager and reconstructs the children
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-add_children"></a>}}
\if{latex}{\out{\hypertarget{method-add_children}{}}}
\subsection{Method \code{add_children()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{winik_manager$add_children()}\if{html}{\out{</div>}}
}

\subsection{Details}{
This is typically called when loading winiks from disk for the first time.
When children are created during the simulation, the family connections are made
through the winik class and added to the manager via add_winik.
}

\subsection{Returns}{
None
Loads winiks from disk.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-load"></a>}}
\if{latex}{\out{\hypertarget{method-load}{}}}
\subsection{Method \code{load()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{winik_manager$load(file_name)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{file_name}}{The location of the file holding the winiks.}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Populates the winik manager with a set of winiks defined in a csv file.
}

\subsection{Returns}{
None
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{winik_manager$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
