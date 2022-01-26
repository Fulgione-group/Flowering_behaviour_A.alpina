#!/bin/bash

######
###
#		Call pairwise differences on the Arabis alpina samples
# 		Runs in parallel across rows of samples
#		Run as:
# 		
# 		for i in {3..81}
#		do
#			./pi_matrix_alpina.command ${i}
#		done
###
######

matrix="Path to SNP matrix file"
results="Path to results file" 
mkdir -p ${results}

java -Xmx4G -classpath ~/java/lib/junit.jar:~/java/lib/jbzip2-0.9.1.jar:~/java/lib/args4j-2.0.12.jar:~/java/lib/commons-compress-1.0.jar:~/java/lib/gson-1.6-javadoc.jar:~/java/lib/gson-1.6-sources.jar:~/java/lib/gson-1.6.jar:bin c.e.data_processing.Pairwise_shore_general ${matrix} ${1} ${results}

######
###
#	In the results file, this creates separate files for separate rows of a matrix of pairwise differences.
#	Just < cat > the rows together in order (do NOT rely on < ls > - spend a few more minutes and make sure they are in numerical order)
#	Mind that the first columns of the SNP matrix are irrelevant (Chromosome \t position \t reference allele)
###
######

