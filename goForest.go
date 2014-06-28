package main

import (
	"fmt"
	"os"
	"strconv"
)

type Forest struct {
	goats  uint32
	wolves uint32
	lions  uint32
}

func (aFor Forest) forestStable() bool {
	if aFor.goats == 0 {
		return (aFor.wolves == 0) || (aFor.lions == 0)
	}
	return (aFor.wolves == 0) && (aFor.lions == 0)
}

func (aFor Forest) forestInvalid() bool {
	return (aFor.goats < 0 || aFor.wolves < 0 || aFor.lions < 0)
}

func meals(forests []Forest) []Forest {
	uForests := make([]Forest, 0, len(forests) * 3)
	m := make(map[Forest]struct{})
	for _, v := range forests {
		m[Forest{v.goats-1, v.wolves-1, v.lions+1}] = struct{}{}
		m[Forest{v.goats-1, v.wolves+1, v.lions-1}] = struct{}{}
		m[Forest{v.goats+1, v.wolves-1, v.lions-1}] = struct{}{}
	}
	for k := range m {
		uForests = append(uForests,k)
	}
	return uForests
}

func devouringPossible(forests []Forest) bool {
	if len(forests) > 0 {
		for _, v := range forests {
			if v.forestStable() {
				return false
			}
		}
		return true
	}
	return false
}

func stableForests(forests []Forest) []Forest {
	stableForests := make([]Forest, 0, len(forests)*3)
	for _, v := range forests {
		if v.forestStable() {
			stableForests = append(stableForests, v)
		}
	}

	return stableForests
}

func findStableForests(forest Forest) []Forest {
	forests := make([]Forest, 0, 3)
	forests = append(forests, forest)
	for devouringPossible(forests) {
		forests = meals(forests)
	}
	return stableForests(forests)
}

func main() {
	if len(os.Args) != 4 {
		fmt.Println("Error: needs three arguments; goats, wolves, lions.")
		return
	}
	goats, err := strconv.Atoi(os.Args[1])
	if err != nil {
		fmt.Println("Error: incorrect goat entry.")
	}
	wolves, err := strconv.Atoi(os.Args[2])
	if err != nil {
		fmt.Println("Error: incorrect wolves entry.")
	}
	lions, err := strconv.Atoi(os.Args[3])
	if err != nil {
		fmt.Println("Error: incorrect lions entry.")
	}
	initialForest := Forest{uint32(goats), uint32(wolves), uint32(lions)}
	stableForests := findStableForests(initialForest)
	if len(stableForests) < 1 {
		fmt.Println("no stable forests found.")
	} else {
		fmt.Println("Stable forest found:")
		for _, aFor := range stableForests {
			fmt.Printf("%v\n", aFor)
		}
	}
}
