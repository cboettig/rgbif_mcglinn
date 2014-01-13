Phylogenetic Resources

Authors:
David C. Tank, Jonathan M. Eastman, Jeremy M. Beaulieu, Stephen A. Smith

Description:
This archive contains datasets and resulting trees for maximum likelihood phylogeny reconstruction and time-scaling.

Notes:
trees: all tree files are scaled to units of millions of years. Internal node labels for higher
taxa are derived from Cantino et al. 2007, Bremer et al. 2009, and Soltis et al. 2011.

Files:
sequence: folder with sequence data and by-gene partition file for RAxML analysis
	Vascular_Plants_32223_taxon.phy: phylip file for RAxML
	Vascular_Plants_32223_taxon.partitions: partitions file for RAxML
Soltis_etal_639taxa_8cploci_rooted.dated.tre: time-scaled reference tree for
	Congruification analyses
Vascular_Plants_rooted.dated.tre: time-scaled MLE megaphylogeny
bootstrap: folder with 100 time-scaled bootstrap replicates of the megaphylogeny
bootstrap.pdf: summary of bootstrap proportions plotted on the MLE
	empty: 100
	white: 90 or better
	gray: 80 or better
	black: 79 or below

Methods:
GenBank accessions for 7 gene regions (18S rDNA, 26S rDNA, ITS, matK, rbcL, atpB, and trnL-F) for 32,223 land plant species were retrieved, cleaned, and assembled into multiple sequence alignments using the PHLAWD pipeline (vers. 3.3a; Smith et al. 2009). We generated nucleotide alignments with mafft45 (vers. 6.937b) using the l-ins-i algorithm. 

We used maximum likelihood as the optimality criterion for tree estimation. Concatenated datasets were analyzed using RAxML (vers. 7.4.1; Stamatakis 2006; Ott et al. 2007). Substitution models were unlinked across gene regions, and branch lengths were optimized under a general time-reversible model with gamma-distributed rate heterogeneity (GTRGAMMA). We constrained searched tree space based on several recent phylogenetic systematic treatments of plants. A total of 427 bipartitions were constrained, including recognized families, orders, and higher-level clades circumscribed by Soltis et al. (2011), APGIII (Bremer et al. 2009), and Cantino et al. (2007). 
 
We scaled the maximum-likelihood estimate (MLE) of the phylogeny to time using Congruification (Eastman et al. 2013). This method resolves topological consistencies between two trees with the aim of mapping dates from a timetree to concordant nodes in an unscaled tree. Our divergence time estimates are derived from a reanalysis of the broadly sampled Soltis et al. (2011) dataset. Because data from the chloroplast represented the largest and most complete partition in this study, we excluded both mitochondrial and nuclear regions from the original dataset. As such, our estimate of the tree involved 639 (of the original 640) species; the excluded taxon, Idria, lacked both nuclear and plastid data. Our reanalysis involved the use of a by-gene partitioned dataset for atpB, matK, ndhF, psbBTNH, rbcL, rpoC2, rps16, and rps4 with unlinked models of substitution across these distinct chloroplast regions. A maximum likelihood tree search was performed using RAxML (vers. 7.4.1; Stamatakis 2006; Ott et al. 2007). Because our interests here were to estimate divergence times using this well-resolved and well-supported topology, and not to reassess this phylogenetic hypothesis, we fixed the topology. We time-scaled the maximum-likelihood estimate using 39 fossil calibrations. Rate smoothing was conducted by penalized likelihood (treePL; Smith and O'Meara 2012) using a smoothing parameter of 0.1 that was optimized on the maximum likelihood estimate.

This reference timetree was used to time-scale the more densely sampled MLE and an associated set of bootstrap trees using Congruification (Eastman et al. 2013). After identifying up to 410 concordant nodes between each of these target trees and the reference timetree, we used penalized-likelihood rate-smoothing to generate a distribution of 100 time-scaled trees from the bootstrap set and a time-scaled MLE. A smoothing parameter of 0.1 was optimized on the MLE and applied for all time scaling.

References:
Bremer, B. et al. An update of the Angiosperm Phylogeny Group classification for the orders and families of flowering plants: APG III. Botanical Journal of the Linnean Society 161, 105-121 (2009).

Cantino, P. D. et al. Towards a phylogenetic nomenclature of Tracheophyta. Taxon 56, 822-846 (2007).

Eastman, J. M., Harmon, L. J. & Tank, D. C. Congruification: support for time scaling large phylogenetic trees. Methods Ecol. Evol. 4, 688-691 (2013).

Ott, M., Zola, J., Stamatakis, A. & Aluru, S. Large-scale maximum likelihood-based phylogenetic analysis on the IBM BlueGene/L. in Proc. 2007 ACMIEEE Conf. Supercomput. 4:1-4:11 (2007).

Smith, S., Beaulieu, J. & Donoghue, M. Mega-phylogeny approach for comparative biology: an alternative to supertree and supermatrix approaches. BMC Evol. Biol. 9, 37 (2009).

Smith, S. A. and O'Meara, B. C. treePL: divergence time estimation using penalized likelihood for large phylogenies. Bioinformatics 28, 2689-2690 (2012).

Soltis, D. E. et al. Angiosperm phylogeny: 17 genes, 640 taxa. Am. J. Bot. 98, 704-730 (2011).

Stamatakis, A. RAxML-VI-HPC: Maximum likelihood-based phylogenetic analyses with thousands of taxa and mixed models. Bioinformatics 22, 2688-2690 (2006).






