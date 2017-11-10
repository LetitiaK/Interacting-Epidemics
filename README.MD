# Interacting Epidemics

Increased susceptibility (see Section 5.2.2) is very common in both the biological as well as
the malware context and is responsible for the increased prevalence and spreading dynamics of
certain infectious diseases and malicious software programs (see Section 5.2.2). Therefore, this
interaction type is considered in more detail within this chapter. In particular, an existing model
for increased susceptibility of infectious diseases is adjusted in order to meet the requirements
to model two malicious software programs (e.g., Pony Stealer and Angler, see Section 5.2.2))
spreading across the same network.

# The SIR x SIR Model


As described in Section 5.2.2.3, Chen et al. proposed a simple SIR model (see Section 3.1.2.2)
of fixed population size with the two diseases A and B [CGCG13]. They proposed that “the
infection rate for disease A is increased, if the individual has or had disease B and vice versa”
[CGCG13]. In this model, the authors only consider cases of super-infection and do not include
cases of co-infection. Furthermore, they assume that an individual cannot recover simultaneously
from both infections in case of a dual infection. Hence, for each individual, there are nine
possible states and two different infection rates. This is depicted as a flow chart with the nine
states S, A, B, AB, a, b, aB, Ab, ab in Figure 24.
S
A B
AB
aB Ab
ab
a b
Figure 24.: Flow Chart according to [CGCG13]
In this Figure, S refers to the state in which an individual is susceptible to both diseases. The upper
case letters represent a current infection and the lower case letters represent a past infection
thereby implying immunity against this disease. For example, if an individual is in state Ab, this
means that the individual is currently infected with disease A and is immune against disease B
54
due to the cure of a past infection with disease B. The transition from state S to state A or state B
occurs with the normal infection rate , which is visualized by red arrows. The transition from
the states A, B, b and a to the states AB, aB and Ab occur, however, with the increased infection
rate , which is visualized by purple arrows. Finally, the black arrows symbolize the recovery
from a disease. The normal infection rate, the increased infection rate and the recovery rate are
equal for both diseases.
In order to fit this SIR  SIR model to the context of malware, several modifications were
performed, which are visualized in Figure 25. First, a direct transition from the susceptible
state S to the dually infected state AB was added in order to integrate co-infections into the
model. This transition occurs with the normal infection rate  and is hence visualized as red
arrow. Accordingly, also a direct recovery from the infected state AB to the recovered state
ab was added. Second, the infection rate is only increased if the computer has malware A.
This restriction was made for two reasons. First, if a computer system has recovered from an
infection with immunity, e.g. by deleting the malware and protecting the system from future
infections through an anti-malware software, this past infection does not have any influence on
future infections. Hence, the transitions from states a and b to the states aB and Ab occur with
the normal instead of the increased transmission rate (visualized by red arrows).
S
A B
AB
aB Ab
ab
a b
Figure 25.: Modified Flow Chart based on [CGCG13]
Second, in most cases the interactions between two diseases or malicious software programs
are not mutual, but rather unilateral (see examples in Section 5.2.2). Accordingly, only the
transition from state A to state AB occurs with the increased transition rate  (visualized by the
purple arrow). The actual infection and recovery rates, which were used for the simulations, are
depicted in Figure 26. The normal infection probability  was set to 0.13 (i.e. 13%) based on
the average percentage of people who click on malicious links or open malicious attachments
in phishing e-mails (see, e.g., [Ver16]). The increased infection probability  was set to 0.40
(i.e. 40%) based on the attack success rate of wide-spread exploit kits such as Angler (see, e.g.
[Cis15]). The simulations were conducted with two different recovery rates, i.e. 0.04 and 0.14
in order to simulate infections that last for one week (1/7) as well as infections that last for four
weeks (1/28).
