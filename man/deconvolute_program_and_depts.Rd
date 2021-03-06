% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sdpCleaner.R
\name{deconvolute_program_and_depts}
\alias{deconvolute_program_and_depts}
\title{Use incorrect department values to assign correct department and program values}
\usage{
deconvolute_program_and_depts(courses, dept_program_map, course_code_colname)
}
\arguments{
\item{courses}{(tibble)}

\item{dept_program_map}{(tibble)}

\item{course_code_colname}{(character)

The dept_name field often contains a program name (e.g. "Igbo") that needs to
moved into a program name column and mapped to a proper department name
(e.g. "African and African American Studies"). Other courses have department
name and program name that should be equal (e.g. "Linguistics").}
}
\description{
Use incorrect department values to assign correct department and program values
}
