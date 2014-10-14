\name{MetReportNames}
\alias{MetReportNames}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
MetReportNames cleans results obtained with the Automated Mass Spectral Deconvolution and Identification System (AMDIS).
}
\description{
MetReportNames automatically process ADMIS results keeping only one compound for each retention time and by assigning the area or the base peak of each compound accross samples. 
}
\usage{
MetReportNames(
	data, 
	AmdisReport, 
	base.peak = FALSE, 
	save = TRUE, 
	folder, 
	output = "metab_data", 
	TimeWindow = 2.5, 
	Remove)
}
\arguments{
  \item{data}{
A character vector defining the names of the samples to be extracted from the AMDIS report under analysis. If missing, a dialog box will pop up allowing the user to select the samples to be extracted.
}
  \item{AmdisReport}{
The default behaviour is to allow the user to point to a .csv file interactively from a popup dialog box. Alternatively, AmdisReport can take a character string indicating the path to the AMDIS report generated in batch mode (See details).  
}
  \item{base.peak}{
If TRUE, the base.peak of each compound is returned. If FALSE, the area of each compound is returned.
}
  \item{save}{
If TRUE (default), a data frame is saved to a .csv file in the dataFolder.
}
  \item{folder}{
A character string pointing to the folder where the results will be saved.
}
  \item{output}{
A character string with the name of the .csv file produced.
}
  \item{TimeWindow}{
A numeric vector defining the maximum allowed different between expected and observed retention. Any compound showing a difference between expected and observed retention higher than the value defined through TimeWindow will be removed from results.
}
  \item{Remove}{
A character string with the names of compounds to be skipped during analysis. Compounds defined through remove (e.g. remove = c("Zylene1", "Pyridine")) will not be considered during analysis.
}
}
\details{
Metab is an R package for processing metabolomics data previously analysed by the Automated Mass Spectral Deconvolution and Identification System (AMDIS) (http://chemdata.nist.gov/mass-spc/amdis/downloads/). AMDIS is one of the most used software for deconvoluting and identifying metabolites analysed by Gas Chromatography - Mass Spectrometry (GC-MS). It is execellent in deconvoluting chromatograms and identifying metabolites based on a spectral library, which is a list of metabolites with their respective mass spectrum and their associated retention times. Although AMDIS is widely and successfully applied to chemistry and many other fields, it shows some limitations when applied to biological studies. First, it generates results in a single spreadsheet per sample, which means that one must manually merge the results provided by AMDIS in a unique spreadsheet for performing further comparisons and statistical analysis, for example, comparing the abundances of metabolites across experimental conditions. AMDIS also allows users to generate a single report containing the results for a batch of samples. However, this report contains the results of samples placed on top of each other, which also requires extensive manual process before statistical analysis. In addition, AMDIS shows some limitations when quantifying metabolites. It quantifies metabolites by calculating the area (Area) under their respective peaks or by calculating the abundance of the ion mass fragment (Base.Peak) used as model to deconvolute the peak associated with each specific metabolite. As the area of a peak may be influenced by coelution of different metabolites, the abundance of the most abundant ion mass fragment is commonly used for quantifying metabolites in biological samples. However, AMDIS may use different ion mass fragments for quantifying the same metabolite across samples, which indicates that using AMDIS results one is not comparing the same variable across experimental conditions. Finally, according to the configurations used when applying AMDIS, it may report more than one metabolite identified for the same retention time. Therefore, AMDIS data requires manual inspection to define the correct metabolite to be assigned to each retention time.
MetReportNames processes an AMDIS report produced in batch mode by selecting the most probable metabolite associated to each retention time, extracting their base.peak or their area and by combining results in a single spreadsheet and in a format that suits further data processing. In order to select the most probable metabolite associated to each retention time, MetReportNames considers the number of question marks reported by AMDIS, which indicates its certainty in identification, and the difference between expected and observed retention times associated with each metabolite.
See below examples of MetReportNames applications. 
}
\value{
MetReportNames generates a data frame containing the metabolites identified in the analyzed sample and their respective abundancies/intensities. See data(exampleMetReport) to see an example of the data frame produced by MetReport. If save = TRUE, MetReportNames also generates a log file with the parameters used in the analysis.

Note that the first line of the resulting data.frame is used to represent sample meta-data (for example replicates). 
}
\references{
Aggio, R., Villas-Boas, S. G., & Ruggiero, K. (2011). Metab: an R package for high-throughput analysis of metabolomics data generated by GC-MS. Bioinformatics, 27(16), 2316-2318. doi: 10.1093/bioinformatics/btr379
}
\author{
Raphael Aggio <ragg005@aucklanduni.ac.nz>
}
\seealso{
\code{\link{htest}}, 
\code{\link{MetReport}},  
\code{\link{normalizeByBiomass}}, 
\code{\link{normalizeByInternalStandard}}, 
\code{\link{removeFalsePositives}}, 
\code{\link{buildLib}} 
}
\examples{
#### Load exmaple of AMDIS report #####
data(exampleAMDISReport)

#### Example 1 ###

#### Filter files "130513_REF_SOL2_2_100_1" and "130513_REF_SOL2_2_100_2" from AMDIS report ##############
#### using a difference between expected and real RT of 0.5min and obtaining the base.peaks ##############
#### of each compound.
test <- MetReportNames(
	data = c("130513_REF_SOL2_2_100_1", "130513_REF_SOL2_2_100_2"), 
	exampleAMDISReport, 
	save = FALSE, 
	TimeWindow = 0.5, 
	base.peak = TRUE)
print(test)

#### Example 2 ###

#### Filter files "130513_REF_SOL2_2_100_1" and "130513_REF_SOL2_2_100_2" from AMDIS report  ##############
#### using a difference between expected and real RT of 1 min and obtaining the AREA of each ##############
#### compound.
test <- MetReportNames(
	data = c("130513_REF_SOL2_2_100_1", "130513_REF_SOL2_2_100_2"), 
	exampleAMDISReport, 
	save = FALSE, 
	TimeWindow = 0.5, 
	base.peak = FALSE)
print(test)

}