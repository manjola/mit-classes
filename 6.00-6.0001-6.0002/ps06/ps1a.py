###########################
# 6.0002 Problem Set 1a: Space Cows 
# Name: MM
# Collaborators:
# Time:

from ps1_partition import get_partitions
import time

#================================
# Part A: Transporting Space Cows
#================================

# Problem 1
def load_cows(filename):
    """
    Read the contents of the given file.  Assumes the file contents contain
    data in the form of comma-separated cow name, weight pairs, and return a
    dictionary containing cow names as keys and corresponding weights as values.

    Parameters:
    filename - the name of the data file as a string

    Returns:
    a dictionary of cow name (string), weight (int) pairs
    """
    cow_d = {}
    placeholder = open(filename, 'r')
    for line in placeholder:
        x = line.strip().split(",")
        cow_d[x[0]] = int(x[1])
    placeholder.close()
    return cow_d
        
# Problem 2
def greedy_cow_transport(cows,limit=10):
    """
    Uses a greedy heuristic to determine an allocation of cows that attempts to
    minimize the number of spaceship trips needed to transport all the cows. The
    returned allocation of cows may or may not be optimal.
    The greedy heuristic should follow the following method:

    1. As long as the current trip can fit another cow, add the largest cow that will fit
        to the trip
    2. Once the trip is full, begin a new trip to transport the remaining cows

    Does not mutate the given dictionary of cows.

    Parameters:
    cows - a dictionary of name (string), weight (int) pairs
    limit - weight limit of the spaceship (an int)
    
    Returns:
    A list of lists, with each inner list containing the names of cows
    transported on a particular trip and the overall list containing all the
    trips
    """
    trips = []
    trip = []
    trip_weight = 0
    sorted_cows = sorted(cows.keys(), key= lambda x: cows[x], reverse = True)
    new_sorted_cows = []

    while sorted_cows != []:
        for i in range(len(sorted_cows)):
            cow = sorted_cows[i]
            cow_weight = cows[cow]
            
            if trip_weight + cow_weight <= limit:
                trip.append(cow)
                trip_weight+=cow_weight
            else:
                new_sorted_cows.append(cow)
                
        trips.append(trip)
        trip = []
        sorted_cows = new_sorted_cows
        new_sorted_cows = []
        trip_weight = 0

    return trips
    
# Problem 3
def brute_force_cow_transport(cows,limit=10):
    """
    Finds the allocation of cows that minimizes the number of spaceship trips
    via brute force.  The brute force algorithm should follow the following method:

    1. Enumerate all possible ways that the cows can be divided into separate trips 
        Use the given get_partitions function in ps1_partition.py to help you!
    2. Select the allocation that minimizes the number of trips without making any trip
        that does not obey the weight limitation
            
    Does not mutate the given dictionary of cows.

    Parameters:
    cows - a dictionary of name (string), weight (int) pairs
    limit - weight limit of the spaceship (an int)
    
    Returns:
    A list of lists, with each inner list containing the names of cows
    transported on a particular trip and the overall list containing all the
    trips
    """
    trips = []
    key_cows = cows.keys()
    num_trips = len(key_cows)

    def trip_weight(x):
        w = 0
        for i in x:
            w += cows[i]
        return w
    
    for t in get_partitions(key_cows):
        L = len(t)
        for t_s in t:
            if trip_weight(t_s) > limit:
                L = len(key_cows) + 1
        if L < num_trips:
            num_trips = L
            trips = t
    return trips
            
            
# Problem 4
def compare_cow_transport_algorithms():
    """
    Using the data from ps1_cow_data.txt and the specified weight limit, run your
    greedy_cow_transport and brute_force_cow_transport functions here. Use the
    default weight limits of 10 for both greedy_cow_transport and
    brute_force_cow_transport.
    
    Print out the number of trips returned by each method, and how long each
    method takes to run in seconds.

    Returns:
    Does not return anything.
    """
    cows = load_cows('ps1_cow_data.txt')
    
    start1 = time.time()
    greedy_trips = greedy_cow_transport(cows,limit=10)
    end1 = time.time()
    print("Greedy Cow,", "number_trips =", len(greedy_trips), ",", "time =", end1-start1)

    start2 = time.time()
    brute_trips = brute_force_cow_transport(cows, limit=10)
    end2 = time.time()
    print("Brute Cow,", "number_trips =", len(brute_trips), ",", "time =", end2-start2)
