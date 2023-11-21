package main

import (
	"fmt"
	"math"
)

// Supply and Demand are slices of integers
type Supply []int
type Demand []int

// CostMatrix is a 2D slice of integers
type CostMatrix [][]int

// Function to check if the problem is balanced
func isBalanced(supply Supply, demand Demand) bool {
	totalSupply := 0
	totalDemand := 0
	for _, s := range supply {
		totalSupply += s
	}
	for _, d := range demand {
		totalDemand += d
	}
	return totalSupply == totalDemand
}

// Function for North-West Corner Method
func northWestCornerMethod(supply Supply, demand Demand, costMatrix CostMatrix) [][]int {
	allocation := make([][]int, len(supply))
	for i := range allocation {
		allocation[i] = make([]int, len(demand))
	}

	i, j := 0, 0
	for i < len(supply) && j < len(demand) {
		min := math.Min(float64(supply[i]), float64(demand[j]))
		allocation[i][j] = int(min)
		supply[i] -= int(min)
		demand[j] -= int(min)
		if supply[i] == 0 {
			i++
		} else {
			j++
		}
	}
	return allocation
}

// Function for Vogel’s Approximation Method
// Function for Vogel’s Approximation Method
func vogelsApproximationMethod(supply Supply, demand Demand, costMatrix CostMatrix) [][]int {
	allocation := make([][]int, len(supply))
	for i := range allocation {
		allocation[i] = make([]int, len(demand))
	}

	for {
		if len(supply) == 0 || len(demand) == 0 {
			break
		}

		rowPenalty := make([]float64, len(supply))
		colPenalty := make([]float64, len(demand))

		// Calculate Row Penalties
		for i, row := range costMatrix {
			min1, min2 := math.MaxInt64, math.MaxInt64
			for _, cost := range row {
				if cost < min1 {
					min2 = min1
					min1 = cost
				} else if cost < min2 {
					min2 = cost
				}
			}
			if min2 == math.MaxInt64 {
				min2 = min1
			}
			rowPenalty[i] = float64(min2 - min1)
		}

		// Calculate Column Penalties
		for j := 0; j < len(demand); j++ {
			min1, min2 := math.MaxInt64, math.MaxInt64
			for i := 0; i < len(supply); i++ {
				cost := costMatrix[i][j]
				if cost < min1 {
					min2 = min1
					min1 = cost
				} else if cost < min2 {
					min2 = cost
				}
			}
			if min2 == math.MaxInt64 {
				min2 = min1
			}
			colPenalty[j] = float64(min2 - min1)
		}

		// Find the max penalty
		maxPenalty, isRow := -math.MaxFloat64, true
		index := -1
		for i, penalty := range rowPenalty {
			if penalty > maxPenalty {
				maxPenalty = penalty
				index = i
				isRow = true
			}
		}
		for j, penalty := range colPenalty {
			if penalty > maxPenalty {
				maxPenalty = penalty
				index = j
				isRow = false
			}
		}

		// Allocate supply to demand
		minCost := math.MaxInt64
		ix, jx := -1, -1
		if isRow {
			for j, cost := range costMatrix[index] {
				if cost < minCost {
					minCost = cost
					ix = index
					jx = j
				}
			}
		} else {
			for i, row := range costMatrix {
				cost := row[index]
				if cost < minCost {
					minCost = cost
					ix = i
					jx = index
				}
			}
		}

		minSupplyDemand := math.Min(float64(supply[ix]), float64(demand[jx]))
		allocation[ix][jx] = int(minSupplyDemand)
		supply[ix] -= int(minSupplyDemand)
		demand[jx] -= int(minSupplyDemand)

		if supply[ix] == 0 {
			// Remove the row from the cost matrix
			costMatrix = append(costMatrix[:ix], costMatrix[ix+1:]...)
			supply = append(supply[:ix], supply[ix+1:]...)
			rowPenalty = append(rowPenalty[:ix], rowPenalty[ix+1:]...)
		}
		if demand[jx] == 0 {
			// Remove the column from the cost matrix
			for i := range costMatrix {
				costMatrix[i] = append(costMatrix[i][:jx], costMatrix[i][jx+1:]...)
			}
			demand = append(demand[:jx], demand[jx+1:]...)
			colPenalty = append(colPenalty[:jx], colPenalty[jx+1:]...)
		}
	}

	return allocation
}

// Russell's Approximation Method
func russellsApproximationMethod(supply Supply, demand Demand, costMatrix CostMatrix) [][]int {
	allocation := make([][]int, len(supply))
	for i := range allocation {
		allocation[i] = make([]int, len(demand))
	}

	for {
		u := make([]float64, len(supply))
		v := make([]float64, len(demand))
		for i := range u {
			u[i] = -1
		}
		for i := range v {
			v[i] = -1
		}

		u[0] = 0 // Starting potential value
		for {
			isChanged := false
			for i, row := range costMatrix {
				for j, cost := range row {
					if allocation[i][j] != 0 || u[i] == -1 && v[j] == -1 {
						continue
					}
					if u[i] != -1 {
						v[j] = float64(cost) - u[i]
						isChanged = true
					} else if v[j] != -1 {
						u[i] = float64(cost) - v[j]
						isChanged = true
					}
				}
			}
			if !isChanged {
				break
			}
		}

		// Finding the maximum difference
		maxDiff, maxI, maxJ := -math.MaxFloat64, -1, -1
		for i, row := range costMatrix {
			for j, cost := range row {
				if u[i] == -1 || v[j] == -1 {
					continue
				}
				diff := float64(cost) - (u[i] + v[j])
				if diff > maxDiff {
					maxDiff, maxI, maxJ = diff, i, j
				}
			}
		}

		if maxDiff == -math.MaxFloat64 {
			break
		}

		// Making the allocation
		allocationValue := math.Min(float64(supply[maxI]), float64(demand[maxJ]))
		allocation[maxI][maxJ] = int(allocationValue)
		supply[maxI] -= int(allocationValue)
		demand[maxJ] -= int(allocationValue)

		if supply[maxI] == 0 {
			for i := range costMatrix {
				costMatrix[i][maxJ] = math.MaxInt64 // Set high cost to ignore this column
			}
		}
		if demand[maxJ] == 0 {
			for j := range costMatrix[maxI] {
				costMatrix[maxI][j] = math.MaxInt64 // Set high cost to ignore this row
			}
		}
	}

	return allocation
}

func main() {
	// Example input
	supply := Supply{20, 30, 25}
	demand := Demand{10, 25, 25, 15}
	costMatrix := CostMatrix{
		{8, 6, 10, 9},
		{9, 12, 13, 7},
		{14, 9, 16, 5},
	}

	// Check if the problem is balanced
	if !isBalanced(supply, demand) {
		fmt.Println("The problem is not balanced!")
		return
	}

	// Calculate solutions
	solutionNW := northWestCornerMethod(supply, demand, costMatrix)
	solutionVA := vogelsApproximationMethod(supply, demand, costMatrix)
	solutionRA := russellsApproximationMethod(supply, demand, costMatrix)

	// Output the results
	fmt.Println("North-West Corner Method Solution:", solutionNW)
	fmt.Println("Vogel's Approximation Method Solution:", solutionVA)
	fmt.Println("Russell's Approximation Method Solution:", solutionRA)
}
