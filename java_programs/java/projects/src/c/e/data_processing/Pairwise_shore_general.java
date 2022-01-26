package c.e.data_processing;

import java.io.File;
import java.lang.Math;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.Writer;
import java.util.*;
import java.io.*;
import java.lang.reflect.Constructor;

public class Pairwise_shore_general {
	
	public Pairwise_shore_general() {}
	
	public void setMatrixFile(String matrixName, String accessionString, String results){
		
		try {
                        int accessionIndex = Integer.parseInt(accessionString) + 3;
                        // int accession = Integer.parseInt(accessionString);

			System.out.println("src/c/e/data_processing/Pairwise_shore.java");			
                        System.out.println("Running on sample: " + accessionIndex);


	    	
	    		// Begin with the big matrix
	    		File matrix = new File(matrixName);
	    		Scanner scannerMatrix = new Scanner(matrix);
	    	
	    		// Just get ID order in the matrix from first line
			String idsUnsplit = scannerMatrix.nextLine();
	    		String[] splitIDs = idsUnsplit.split("\t");
			
			
			// Build IndexesToGet, array of interesting indexes 
			
			int allPlants = splitIDs.length - 3;

			int howMany = allPlants;	// canarianIndexes.length + moroccanIndexes.length + iberianIndexes.length; // + capeVerdeanIndexes.length + madeiranIndexes.length 
			System.out.println("howMany: "+ howMany);
			
			int[] IndexesToGet = new int[howMany];
			
			for (int i=3; i<splitIDs.length; i++) {	
				IndexesToGet[i-3] = i;
			}
	
			//
			// Now the real game
			// 

        	// Begin to calculate pairwise differences
        	
			int[] differences = new int[IndexesToGet.length];
			int[] length = new int[IndexesToGet.length];
			String chr = "chr0";
			
		    	matrix = new File(matrixName);
		    	scannerMatrix = new Scanner(matrix);
		    	scannerMatrix.nextLine();
	    	
			while ( scannerMatrix.hasNextLine() ) {
				String snp = scannerMatrix.nextLine();
				String[] splitSnp = snp.split("\t");
				
				// Just print where we are
        			if (!chr.equals(splitSnp[0])) {
        				System.out.println("Chr: " + splitSnp[0]);
        			}
        			chr = splitSnp[0];
        			int pos = Integer.parseInt(splitSnp[1]);
	       		 	//
	        		
       		 		// if (!mask[chr-1].get(pos)) {
	
    		        	char base1 = splitSnp[accessionIndex].charAt(0);						// IndexesToGet[accession]].charAt(0);
	    	        	
    			       	for (int acc2=0; acc2<IndexesToGet.length; acc2++) {
    			       		char base2 = splitSnp[IndexesToGet[acc2]].charAt(0);
    			       		
    			       		if ( (base1 != 'N') && (base2 != 'N') ) {
    			       			length[acc2] = length[acc2] + 1;
    		        			if (base1 != base2) {
    			       				differences[acc2] = differences[acc2] + 1;
    			       			}
    			       		}
    		        	}
       	 		//} 		
		        }
			// Calculate pi
			
			double[] pairwDiff = new double[IndexesToGet.length];
			for (int q=0; q<IndexesToGet.length; q++) {
				pairwDiff[q] = (double)(differences[q])/(double)(length[q]);
			}
			
			// Done
			
			
			
			// Write out
			
			Writer writer = new FileWriter(results + "pi" + accessionIndex + ".txt");
			PrintWriter out = new PrintWriter(writer);
			for (int q=0; q<IndexesToGet.length; q++) {
				out.print(pairwDiff[q]);
				out.print("\t");
			}
			out.print("\n");
			out.close();



		
                        Writer writer2 = new FileWriter(results + "len" + accessionIndex + ".txt");
                        PrintWriter out2 = new PrintWriter(writer2);
                        for (int q=0; q<IndexesToGet.length; q++) {
                                out2.print(length[q]);
                                out2.print("\t");
                        }
                        out2.print("\n");
                        out2.close();




                        Writer writer3 = new FileWriter(results + "dif" + accessionIndex + ".txt");
                        PrintWriter out3 = new PrintWriter(writer3);
                        for (int q=0; q<IndexesToGet.length; q++) {
                                out3.print(differences[q]);
                                out3.print("\t");
                        }
                        out3.print("\n");
                        out3.close();




	

			// Write accession names
			if (accessionIndex == 5) {
				Writer writerN = new FileWriter(results + "names.txt");
				PrintWriter outN = new PrintWriter(writerN);
	    	   		for (int sub=0; sub<IndexesToGet.length; sub++) {
					outN.print(splitIDs[IndexesToGet[sub]] + "\n");
				}
	       			outN.print("\n");
				outN.close();
			}




	
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	public static void main(String[] args) {
		Pairwise_shore_general pairwise_shore_general = new Pairwise_shore_general();
		pairwise_shore_general.setMatrixFile(args[0], args[1], args[2]);
	}
}
