% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kerberos_runSpark.R
\name{kerberos_runSpark}
\alias{kerberos_runSpark}
\title{kerberos_runSpark}
\usage{
kerberos_runSpark(
  kerberosUser,
  pysparkPath,
  sparkMaster,
  sparkDeployMode,
  sparkNumExecutors,
  sparkDriverMemory,
  sparkExecutorMemory,
  sparkConfigDynamic
)
}
\arguments{
\item{kerberosUser}{kerberos user}

\item{pysparkPath}{full path to the Pyspark program}

\item{sparkMaster}{sparkMaster master}

\item{sparkDeployMode}{sparkDeployMode}

\item{sparkNumExecutors}{sparkNumExecutors}

\item{sparkDriverMemory}{sparkDriverMemory}

\item{sparkExecutorMemory}{sparkExecutorMemory}

\item{sparkConfigDynamic}{sparkConfigDynamic}
}
\value{

}
\description{
Run Pyspark script from WMF Analytics Client(s)
}
