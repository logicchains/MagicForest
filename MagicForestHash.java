// See http://unriskinsight.blogspot.com/2014/06/fast-functional-goats-lions-and-wolves.html
// Sascha Kratky (kratky@unrisk.com), uni software plus GmbH & MathConsult GmbH
//
// Modified by Jonathan Barnard to be fast.
// compilation requires Java 7

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Iterator;
import java.util.HashSet;

public final class MagicForest {
    
    static final class Forest {
	public int goats;
	public int wolves;
	public int lions;
	
	private Forest(int goats, int wolves, int lions) {
	    this.goats = goats;
	    this.wolves = wolves;
	    this.lions = lions;
	}
	
	static public Forest makeForest(int goats, int wolves, int lions) {
	    return new Forest(goats, wolves, lions);
	}

	public boolean isStable() {
	    if (this.goats == 0) return (this.wolves == 0) || (this.lions == 0);
	    return (this.wolves == 0) && (this.lions == 0);
	}
	
	public boolean isValid() {
	    return !(this.goats < 0 || this.wolves < 0 || this.lions < 0);
	}
		
	@Override
	public String toString() {
	    return "Forest [goats=" + this.goats + ", wolves=" + this.wolves +
		", lions=" + this.lions + "]";
	}
	
    }

    static Forest[] meal(Forest[] forests) {
	HashSet<Forest> uniqFors = new HashSet<Forest>(forests.length*3); 
	for(int i = 0, j = 0; i < forests.length; i++, j+=3){
	    uniqFors.add(new Forest(forests[i].goats + 1,
				    forests[i].wolves - 1,
				    forests[i].lions - 1));
	    uniqFors.add(new Forest(forests[i].goats - 1,
				    forests[i].wolves + 1,
				    forests[i].lions - 1));
	    uniqFors.add(new Forest(forests[i].goats - 1,
					    forests[i].wolves - 1,
				    forests[i].lions + 1));
	}
	return uniqFors.toArray(forests);
    }
    
    static boolean devouringPossible(Forest[] forests) {
	if(forests.length>0){
	    for(Forest aForest : forests){
		if(aForest.isStable()){
		  return false;
		}
	    }
	    return true;
	}else{
	    return false;
	}
    }

    static List<Forest> stableForests(Forest[] forests) {
	List<Forest> stableForests = new ArrayList<>(3);
	for(Forest aForest : forests){
	    if(aForest.isStable()){
		stableForests.add(aForest);
	    }
	}
	return stableForests;
    }
    
    static public List<Forest> findStableForests(Forest forest) {
	Forest[] forests = new Forest[1];
	forests[0] = forest;
	while (devouringPossible(forests)){
	    forests = meal(forests);
	}
	return stableForests(forests);
    }
    
    public static void main(String[] args) {
	if (args.length != 3) {
	    System.err.println("USAGE: " + MagicForest.class.getSimpleName() +
			       " <goats> <wolves> <lions>");
	    System.exit(-1);
	}
        long startTime = System.currentTimeMillis();
	try {
	    Forest initialForest = Forest.makeForest(Integer.parseInt(args[0]),
						     Integer.parseInt(args[1]), Integer.parseInt(args[2]));
	    List<Forest> stableForests = findStableForests(initialForest);
	    if (stableForests.isEmpty()) {
		System.out.println("no stable forests found.");
	    }
	    else {
		for(Forest aForest : stableForests){
		    System.out.println(aForest);
		}
	    }
	} catch (Exception ex) {
	    System.err.println("ERROR: " + ex.toString());
	    System.exit(-1);
	}
        System.out.printf("Time taken was %f seconds.\n", (System.currentTimeMillis()-startTime)/1000.0);
    }
    
}
