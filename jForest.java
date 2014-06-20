// See http://unriskinsight.blogspot.com/2014/06/fast-functional-goats-lions-and-wolves.html
// Sascha Kratky (kratky@unrisk.com), uni software plus GmbH & MathConsult GmbH
//
// Modified by Jonathan Barnard to be slightly slower.
// compilation requires Java 7

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Iterator;
import java.util.HashSet;

public final class jForest {
    public final static int BUFFER_SIZE = 1024*500;
    static final class ForList {
	public int forests[];
	public int length;
	public int cap;

	public ForList(int cap){
	    this.length = 0;
	    this.forests = new int[cap*3];
	    this.cap = cap;
	}

	public void clear(){
	    length = 0;
	}

	public void add(int g, int w, int l){
	    int pos = length*3;
	    this.forests[pos] = g;
	    this.forests[pos+1] = w;
	    this.forests[pos+2] = l;
	    this.length++;
	}

	public int gAt(int n){
	    return forests[n*3];
	}

	public int wAt(int n){
	    return forests[n*3+1];
	}

	public int lAt(int n){
	    return forests[n*3+2];
	}
	
	public boolean isStable(int n) {
	    int pos = n*3;
	    if (forests[pos] == 0){ 
		return (forests[pos+1] == 0) || (forests[pos+2] == 0);
	    }
	    return (forests[pos+1] == 0) && (forests[pos+2] == 0);
	}
	
	public  boolean isValid(int n) {
	    int pos = n*3;
	    return !(forests[pos] < 0 || forests[pos+1] < 0 || forests[pos+2] < 0);
	}
		
	public boolean forLessThan(int f1, int f2){
	    int posa = f1*3;
	    int posb = f2*3;
	    boolean res = false;
	    if(forests[posa] == forests[posb]){
		if(forests[posa+1] == forests[posb+1]){
		    if(forests[posa+2] == forests[posb+2]){
			res = false;
		    }else{
			res = forests[posa+2] < forests[posb+2];
		    }
		}
		else {
		    res = forests[posa+1] < forests[posb+1];
		}
	    }else{
		res = forests[posa] < forests[posb];
	    }
	    return res;
	}

	public void swapFors(int f1, int f2){
	    int posa = f1*3;
	    int posb = f2*3;
	    int tmpG = forests[posa];
	    int tmpW = forests[posa+1];
	    int tmpL = forests[posa+2];
	    this.forests[posa] = forests[posb];
	    this.forests[posa+1] = forests[posb+1];
	    this.forests[posa+2] = forests[posb+2];
	    this.forests[posb] = tmpG;
	    this.forests[posb+1] = tmpW;
	    this.forests[posb+2] = tmpL;
	}


	public void quicksort(int low, int high) {
	    int i = low, j = high;	    
	    int pivot = forests[low + (high-low)/2];
	    forests[(this.cap-1)*3] = this.gAt(pivot);
	    forests[(this.cap-1)*3+1] = this.wAt(pivot);
	    forests[(this.cap-1)*3+2]= this.lAt(pivot);	    
	    while (i <= j) {
		while (forLessThan(i, this.cap-1)) {
		    i++;
		}
		while (forLessThan(this.cap-1, j)) {
		    j--;
		}
		if (i <= j) {
		    swapFors(i, j);
		    i++;
		    j--;
		}
	    }
	    if (low < j)
		quicksort(low, j);
	    if (i < high)
		quicksort(i, high);
	}
	public void qsFors(int start, int len){
	    if (len < 2){
		return;
	    }
	    int p = len/2+start;
	    forests[(this.cap-1)*3] = this.gAt(p);
	    forests[(this.cap-1)*3+1] = this.wAt(p);
	    forests[(this.cap-1)*3+2]= this.lAt(p);	    	   
	    int left = start;
	    int right = start + len - 1;
	    while (left <= right) {
		if (forLessThan(left, this.cap-1)) {
		    left++;
		    continue;
		}
		if (forLessThan(this.cap-1, right)) {
		    right--;
		    continue;
		}
		swapFors(left, right);
		left++;
		right--;
	    }
	    qsFors(start, right - start + 1);
	    qsFors(left, start + len - left);
	}
	
	public String printFor(int n) {
	    int pos = n*3;
	    return "Forest [goats=" + this.forests[pos] + ", wolves=" + this.forests[pos+1] +
		", lions=" + this.forests[pos+2] + "]";
	}

    }
    static void uniqueFors(ForList forests, ForList newFors){
	newFors.clear();
	forests.qsFors(0, forests.length);
	newFors.add(forests.gAt(0), forests.wAt(0), forests.lAt(0));
	int prevG = forests.gAt(0);
	int prevW = forests.wAt(0);
	int prevL = forests.lAt(0);
	for(int i = 1; i < forests.length; i++){
	    if(!(prevG == forests.gAt(i)
		 && prevW == forests.wAt(i)
		 && prevL == forests.lAt(i))){
		newFors.add(forests.gAt(i), forests.wAt(i), forests.lAt(i));
		prevG = forests.gAt(i);
		prevW = forests.wAt(i);
		prevL = forests.lAt(i);
	    }
	}
    }	
  
    
    static void meal(ForList forests, ForList newFors) {
	newFors.clear();
	for(int i = 0; i < forests.length; i++){
	    newFors.add(forests.gAt(i)-1, forests.wAt(i)-1, forests.lAt(i)+1);
	    newFors.add(forests.gAt(i)-1, forests.wAt(i)+1, forests.lAt(i)-1);
	    newFors.add(forests.gAt(i)+1, forests.wAt(i)-1, forests.lAt(i)-1);
	}
	/*	for(int i = 0; i < forests.length; i++){
		System.out.println(newFors.printFor(i));
		}*/
	uniqueFors(newFors, forests);
    }    

    static boolean devouringPossible(ForList forests) {
	if(forests.length>0){
	    for(int i = 0; i < forests.length; i++){
		if(forests.isStable(i)){
		  return false;
		}
	    }
	    return true;
	}else{
	    return false;
	}
    }

    static void stableForests(ForList forests, ForList newFors) {
	newFors.clear();
	for(int i = 0; i < forests.length; i++){
	    if(forests.isStable(i)){
		newFors.add(forests.gAt(i), forests.wAt(i), forests.lAt(i));
	    }
	}
    }
    
    public static ForList findStableForests(int g, int w, int l, ForList forests1, ForList forests2) {
	forests1.add(g, w, l);
	while (devouringPossible(forests1)){
	    meal(forests1, forests2);
	}
	stableForests(forests1, forests2);
	return forests2;
    }
    
    public static void main(String[] args) {
	if (args.length != 3) {
	    System.err.println("USAGE: " + jForest.class.getSimpleName() +
			       " <goats> <wolves> <lions>");
	    System.exit(-1);
	}
	ForList forestsArr1 = new ForList(BUFFER_SIZE);
	ForList forestsArr2 = new ForList(BUFFER_SIZE);
        long startTime = System.currentTimeMillis();
	try {
	    ForList stableForests = findStableForests(Integer.parseInt(args[0]),
						      Integer.parseInt(args[1]), 
						      Integer.parseInt(args[2]),
						      forestsArr1, forestsArr2);
	    if (stableForests.length < 1) {
		System.out.println("no stable forests found.");
	    }
	    else {
		for(int i = 0; i < stableForests.length; i++){
		    System.out.printf("%s\n",stableForests.printFor(i));
		}
	    }
	} catch (Exception ex) {
	    System.err.println("ERROR: " + ex.toString());
	    System.exit(-1);
	}
	System.out.printf("Time taken was %f seconds.\n", (System.currentTimeMillis()-startTime)/1000.0);
    }
}
