package main

import(
	"fmt"
	"sort"
	"os"
	"strconv"
)

type Forest struct{
	goats int
	wolves int
	lions int
}

func forestStable(aFor *Forest) bool {
	if (aFor.goats == 0) {
		return (aFor.wolves == 0) || (aFor.lions == 0)
	}
	return (aFor.wolves == 0) && (aFor.lions == 0)
}

func forestInvalid(aFor *Forest) bool {
	return (aFor.goats < 0 || aFor.wolves < 0 || aFor.lions < 0)
}

func compFors(f1, f2 *Forest) bool{
	res := false
	if(f1.goats == f2.goats){
		if(f1.wolves == f2.wolves){
			if(f1.lions == f2.lions){
				res = false
			}else{
				res = f1.lions < f2.lions
			}
		}else {
			res = f1.wolves < f2.wolves
		}
	}else{
		res = f1.goats < f2.goats
	}
	return res
}

type ForList []Forest

func (a ForList) Len() int {
	return len(a) 
}
func (a ForList) Swap(i, j int){
	a[i], a[j] = a[j], a[i] 
}
func (a ForList) Less(i, j int) bool{
	return compFors(&a[i],&a[j])
}

func uniqueFors(forests []Forest) []Forest {
	uForests := make([]Forest,0,len(forests))
	sort.Sort(ForList(forests))
	prevFor := &forests[0]
	uForests = append(uForests, *prevFor)
	for i := 1; i < len(forests); i++{
		if *prevFor != forests[i] && !forestInvalid(&forests[i]) {
			uForests = append(uForests, forests[i])
			prevFor = &forests[i];
		}
	}
	return uForests;
}

func meals(forests []Forest)[]Forest {
	j := 0
	mealedForests := make([]Forest,len(forests)*3)
	for i, _ := range forests{
		mealedForests[j].goats = forests[i].goats -1
		mealedForests[j].wolves = forests[i].wolves -1
		mealedForests[j].lions = forests[i].lions +1
		j++;
		mealedForests[j].goats = forests[i].goats -1
		mealedForests[j].wolves = forests[i].wolves +1
		mealedForests[j].lions = forests[i].lions -1
		j++;
		mealedForests[j].goats = forests[i].goats +1
		mealedForests[j].wolves = forests[i].wolves -1
		mealedForests[j].lions = forests[i].lions -1
		j++;
	}	
	/*
	hash := make(map[Forest]bool)
	for _, forest := range mealedForests {
		hash[forest] = true
	}
	uniqueForests := make([]Forest,0,len(hash))
	for key, _ := range hash{
		uniqueForests = append(uniqueForests, key)
	}
	return uniqueForests*/
	return uniqueFors(mealedForests);
}

func devouringPossible(forests []Forest) bool{
	if len(forests) > 0{
		for i, _ := range forests{
			if forestStable(&forests[i]){
				return false
			}      
		}
		return true
	}else{
		return false
	}
}

func stableForests(forests []Forest) []Forest {
	stableForests := make([]Forest,0,len(forests)*3)
	for i, _ := range forests{
		if(forestStable(&forests[i])){
			stableForests = append(stableForests, forests[i])
		}
	}

	return stableForests
}

func findStableForests(forest Forest)[]Forest {
	forests := make([]Forest,0,3)
	forests = append(forests, forest)
	for ;devouringPossible(forests); {
		forests = meals(forests)
	}
	return stableForests(forests)
}

func main() {
	if (len(os.Args) != 4) {
		fmt.Println("Error: needs three arguments; goats, wolves, lions.");
		return;
	}
	goats, err := strconv.Atoi(os.Args[1])
	if err != nil{
		fmt.Println("Error: incorrect goat entry.")
	}
	wolves, err := strconv.Atoi(os.Args[2])
	if err != nil{
		fmt.Println("Error: incorrect wolves entry.")
	}
	lions, err := strconv.Atoi(os.Args[3])
	if err != nil{
		fmt.Println("Error: incorrect lions entry.")
	}
	initialForest := Forest{goats, wolves, lions}
	stableForests := findStableForests(initialForest)
	if len(stableForests) < 1 {
		fmt.Println("no stable forests found.")
	}else {
		fmt.Println("Stable forest found:")
		for _, aFor := range stableForests{
			fmt.Printf("%v\n", aFor)
		}		
	}
}
