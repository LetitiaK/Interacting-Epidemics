# Interacting Epidemics

Increased susceptibility is very common in both the biological as well as the malware context and is responsible for the increased prevalence and spreading dynamics of certain infectious diseases and malicious software programs. 

In this case, an existing model for increased susceptibility of infectious diseases is adjusted in order to meet the requirements to model two malicious software programs (e.g., Pony Stealer and Angler) spreading across the same network and to run respective simulations. 

# The SIR x SIR Model

Chen et al. proposed a simple SIR model of fixed population size with the two diseases A and B. They proposed that “the infection rate for disease A is increased, if the individual has or had disease B and vice versa”. In this model, the authors only consider cases of super-infection and do not include cases of co-infection. Furthermore, they assume that an individual cannot recover simultaneously from both infections in case of a dual infection. Hence, for each individual, there are nine possible states and two different infection rates. 

In order to fit this SIR x SIR model to the context of malware, several modifications were performed. 

* First, a direct transition from the susceptible state to the dually infected state was added in order to integrate co-infections into the model. This transition occurs with the normal infection rate. Accordingly, also a direct recovery from the dually infected state to the recovered state was added. Second, the infection rate is only increased if the computer has malware A. This restriction was made for two reasons. First, if a computer system has recovered from an infection with immunity, e.g. by deleting the malware and protecting the system from future infections through an anti-malware software, this past infection does not have any influence on future infections.

* Second, in most cases the interactions between two diseases or malicious software programs are not mutual, but rather unilateral. Accordingly, only the transition from state A to state AB occurs with the increased transition rate. The actual infection and recovery rates, which were used for the simulations, are as follows: The normal infection probability was set to 0.13 (i.e. 13%) based on
the average percentage of people who click on malicious links or open malicious attachments in phishing e-mails. The increased infection probability was set to 0.40 (i.e. 40%) based on the attack success rate of wide-spread exploit kits such as Angler. The simulations were conducted with two different recovery rates, i.e. 0.04 and 0.14 in order to simulate infections that last for one week (1/7) as well as infections that last for four weeks (1/28).

# Implementation

## Initialization

The implementation allows to specify several initial conditions for the simulation(s), which are
depicted in Listing 1. In particular, the variables init_inf_A and init_inf_B (see Listing 1, Lines:
2-3) determine the number of initially infected vertices. Note that within this thesis, the number
of initially infected vertices is always equal for both malicious software programs in order to
ensure that differences in the spreading behavior can be ascribed to interactions and are not
due to a larger number of initially infected vertices. For the simulations, the number of initially
infected vertices is either equal to one (i.e. a single vertex) or equal to one percent of the vertices
in the graph.
The variables alpha_A and alpha_B (Lines: 6-7) represent the normal transmission probability,
which is always equal for both malicious software programs within this thesis. The variables
beta_A and beta_B (Lines: 10-11) represent the increase of the transmission probability. This
means that for malware B the increased transmission probability is equal to alpha_B + beta_B
(0.13 + 0.27 = 0.40). For malware A, the increase of the transmission probability is equal
to zero (i.e. no increased transmission probability). Using this approach facilitates later simulations
with mutually increasing susceptibility (or transmissibility) or differently increasing
transmission probabilities.

The variables gamma_A and gamma_B (Lines: 14-15) represent the recovery probability. As
mentioned before, within this thesis the recovery probability is always equal for both malicious
software programs and is either equal to 0.04 or 0.14.
The variables simlength and simnumber (Lines: 18 & 20) determine the number of time steps
and the number of simulations, respectively. The variable plot.spread (Line: 25) is used to
enable or disable the plotting of the graph. This should be set to TRUE only if the graph is
reasonably small.
Finally, the variables recovery.wait_A and recovery.wait_B (Lines: 27-28) are used to determine
whether the initially infected vertices can recover from time step 0 to time step 1. If this variable
is set to FALSE, the initially infected vertices can recover during this first time step, meaning
that the epidemic might immediately die out from time step 0 to time step 1. Within this thesis,
these two variables are always set to TRUE, meaning that the initially infected vertices can only
recover from the second time step onwards (i.e. from time step 1 to time step 2 at the earliest).

## Selection of Edges

In order to implement the interaction between the two malicious software programs A and B (i.e.
the increased transmission probability), the edges, along which the malware can be transmitted
need to be sorted into four different categories. In particular, one differentiates between those
edges along which the malicious software programs A and B are transmitted with the normal
transmission probability and those edges along which A and B are transmitted with an increased
transmission probability. Therefore, the program iterates through the entire edge list of a graph
and sorts the edges based on the schemata given in Figure 27 and Figure 28.
If a vertex that is infected with malware A (state A) is connected with another vertex that is
either not infected at all (state S) or that has already recovered from malware B (state b), then
this vertex transmits malware A with the normal transmission probability. Vice versa, a vertex
infected with malware B (state B) transmits B with normal transmission probability to those
vertices that are either not infected at all (state S) or that have already recovered from malware
A (state a). Figure 27 (a) shows all possible constellations between two vertices for malware A.
Similarly, Figure 27 (b) shows all these possible constellations for malware B28.
If a vertex that is infected with malware A (states A, AB and Ab) is connected to a vertex that is
currently infected with malware B (state B), then this vertex transmits A with an increased probability.
Figure 28 (a) shows all these possible constellations between two vertices for malware
A. Similarly, a vertex that is currently infected with malware B (states B,AB and Ba) transmits
B with an increased transmission probability to those vertices that are currently infected with
malware A (state A). Figure 28 (b) shows all these possible constellations for malware B.

In order to sort the edges accordingly, four lists are created (see Listing 2, Lines: 5-8). Then, a
for-loop is used to iterate through the edge list of the graph (see Listing 2, Line: 12).

The following code (Listing 2, Lines: 14-26) shows exemplarily, how the edge between vertex 1
and 2 in Figure 29 is sorted into the two lists highrisk.edges_A and highrisk.edges_B. As can be
seen in Figure 28 (a) and (b), the edge between vertex 1 and 2 in Figure 29 offers an increased
transmission probability for both malicious software programs A and B.

In particular, the first if-statement (Line 16) is used to verify whether vertex 1 is only infected
with malware A. Therefore, the logical vector of malware A (infected_A) must be TRUE for
vertex 1 and the logical vector of malware B (infected_B) must be FALSE for vertex 129. Similarly,
the second if-statement (Line: 20) is used to validate whether vertex 2 is only infected
with malware B. Subsequently, the edge is saved in the respective lists (Lines: 22-23). For
optimization purposes next (Line: 26) is used to jump directly to the next edge as soon as the
current edge is categorized. Following this approach, suitable if-statements are formulated for
each possibility (see Figure 27 and Figure 28).

## Transmission and Recovery

Both the transmission of the malware as well as the recovery from an infection are modeled
using Bernoulli trials. The outcome of Bernoulli trials can be represented as vectors of predefined
length of independent dichotomous trials with fixed probability of success on each trial.
The outcome of each trial can be either 1 (i.e. success) or 0 (i.e. failure) (for more details, see,
e.g., [DS12]). In this case, a success means either the transmission along a certain edge or the
recovery of a certain vertex. In R, the Bernoulli trial is implemented using the rbinom(N,n,p)
function (binomial distribution) and setting n = 1.
In Listing 3, the transmission process is depicted exemplarily for malware A. The transmission
process for malware B is defined equivalently. As mentioned above, the rbinom(N,n,p) function
is used for selection purposes (see Listing 3, Lines: 3-4). In particular, one differentiates
between those cases, in which N is the number of edges along which the malware is transmitted
with normal transmission probability and p = alpha_A (Line: 3) and those cases, in which
N is the number of edges along which the malware is transmitted with increased transmission
probability and p = alpha_A + beta_A (Line: 4) (for more details see also Section 6.2.2). Those
edges, for which the Bernoulli trials return a success (i.e. are equal to 1) are selected and saved
in the variables transmitter.edges_normal_A and transmitter.edges_high_A, respectively (Lines:
7-10). In the next step, the vertices of the selected edges are extracted and saved as a vector.
In order to avoid duplicate vertices in this vector unique is used (Lines: 13-14). Finally, the
selected vertices are set to TRUE in the logical vector infected_A of malware A (Lines: 17-18).
In Listing 4, the recovery process is depicted exemplarily for malware A (the transmission
process for malware B is defined equivalently). Depending on the variable recovery.wait_A
(see Section 6.2.1) the recovery process is executed during time step 1 or during time step 2 at
the earliest (see Listing 4, Line: 5). If the recovery process is executed, all infected vertices
are selected by picking all those vertices for which the logical vector infected_A of malware A
evaluates to TRUE (Line: 6). Then, a Bernoulli trial (see above) is used to determine, which
vertices will recover during the respective time step. In this case, N is the number of currently
infected vertices and p is the recovery probability gamma_A (Lines: 7-8). Subsequently, those
vertices, for which the Bernoulli trial returns a success (i.e. is equal to 1), are selected (Line: 9).
Because the logical vector infected_A represents only a binary logic, NA is used to implement
a ternary logic. In this way, all three statuses (susceptible, infected and recovered) can be
represented within one vector. Hence, all recovered vertices are set to NA in the logical vector
(Line: 10).

# References
