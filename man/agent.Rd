% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/agent.R
\docType{class}
\name{agent}
\alias{agent}
\title{agent}
\description{
This is an object that represents a villager (agent).
}
\details{
This class acts as an abstraction for handling villager-level logic. It can take a
number of functions that run at each timestep. It also has an associated
}
\section{Methods}{

\describe{
\item{\code{as_table()}}{Represents the current state of the agent as a tibble}
\item{\code{get_age()}}{Returns age in terms of years}
\item{\code{get_gender()}}{}
\item{\code{get_days_sincelast_birth()}}{Get the number of days since the agent last gave birth}
\item{\code{initialize()}}{Create a new agent}
\item{\code{propagate()}}{Runs every day}
}
Create a new agent
}

\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{identifier}}{A unique identifier that can be used to identify and find the agent}

\item{\code{first_name}}{The agent's first name}

\item{\code{last_name}}{The agent's last name}

\item{\code{age}}{The agent's age}

\item{\code{mother_id}}{The identifier of the agent's mother}

\item{\code{father_id}}{The identifier of the agent's father}

\item{\code{profession}}{The agent's profession}

\item{\code{partner}}{The identifier of the agent's partner}

\item{\code{gender}}{The agent's gender}

\item{\code{alive}}{A boolean flag that represents whether the villager is alive or dead}

\item{\code{children}}{A list of children identifiers}

\item{\code{health}}{A percentage value of the agent's current health}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-agent-new}{\code{agent$new()}}
\item \href{#method-agent-is_alive}{\code{agent$is_alive()}}
\item \href{#method-agent-get_days_since_last_birth}{\code{agent$get_days_since_last_birth()}}
\item \href{#method-agent-add_child}{\code{agent$add_child()}}
\item \href{#method-agent-as_table}{\code{agent$as_table()}}
\item \href{#method-agent-clone}{\code{agent$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-agent-new"></a>}}
\if{latex}{\out{\hypertarget{method-agent-new}{}}}
\subsection{Method \code{new()}}{
Used to created new agent objects.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{agent$new(
  identifier = NA,
  first_name = NA,
  last_name = NA,
  age = 0,
  mother_id = NA,
  father_id = NA,
  partner = NA,
  children = vector(mode = "character"),
  gender = NA,
  profession = NA,
  alive = TRUE,
  health = 100
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{identifier}}{The agent's identifier}

\item{\code{first_name}}{The agent's first name}

\item{\code{last_name}}{The agent's last name}

\item{\code{age}}{The age of the agent}

\item{\code{mother_id}}{The identifier of the agent's mother}

\item{\code{father_id}}{The identifier of the agent' father}

\item{\code{partner}}{The identifier of the agent's partner}

\item{\code{children}}{An ordered list of of the children from this agent}

\item{\code{gender}}{The gender of the agent}

\item{\code{profession}}{The agent's profession}

\item{\code{alive}}{Boolean whether the agent is alive or not}

\item{\code{health}}{A percentage value of the agent's current health}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new agent object
A function that returns true or false whether the villager dies
This is run each day
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-agent-is_alive"></a>}}
\if{latex}{\out{\hypertarget{method-agent-is_alive}{}}}
\subsection{Method \code{is_alive()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{agent$is_alive()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A boolean whether the agent is alive (true for yes)
Gets the number of days from the last birth. This is also
the age of the most recently born agent
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-agent-get_days_since_last_birth"></a>}}
\if{latex}{\out{\hypertarget{method-agent-get_days_since_last_birth}{}}}
\subsection{Method \code{get_days_since_last_birth()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{agent$get_days_since_last_birth()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
The number of days since last birth
Connects a child to the agent. This method ensures that the
'children' vector is ordered.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-agent-add_child"></a>}}
\if{latex}{\out{\hypertarget{method-agent-add_child}{}}}
\subsection{Method \code{add_child()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{agent$add_child(child)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{child}}{The agent object representing the child}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
None
Returns a data.frame representation of the agent
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-agent-as_table"></a>}}
\if{latex}{\out{\hypertarget{method-agent-as_table}{}}}
\subsection{Method \code{as_table()}}{
I hope there's a more scalable way to do this in R; Adding every new attribute to this
function isn't practical
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{agent$as_table()}\if{html}{\out{</div>}}
}

\subsection{Details}{
The village_state holds a copy of all of the villagers at each timestep; this method is used to turn
the agent properties into the object inserted in the village_state.
}

\subsection{Returns}{
A data.frame representation of the agent
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-agent-clone"></a>}}
\if{latex}{\out{\hypertarget{method-agent-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{agent$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
