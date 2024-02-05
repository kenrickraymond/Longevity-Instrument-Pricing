This repository contains the resources relevant to my thesis. It builds upon the [StMoMo package](https://github.com/amvillegas/StMoMo), using a simulation-based approach to price derivatives indexed on the number of survivors from a reference population. Specifically, it prices:
* Survival forwards (s-forwards)
* Survivor swaps

Using eight pricing premium principles:
* Wang Transform
* Proportional Hazard Transform
* Dual Power Transform
* Gini Priciple
* Exponential Transform
* Standard Deviation Principle
* Variance Principles
* Mean Absolute Deviation (MAD) Principle

Additionally, the thesis analyzes the impact of the choice of mortality model and premium principle to the risk-measures associated with the derivatives a Monte-Carlo approach. Specifically, the risk-measures considered include:
* Value-at-risk (VaR)
* Expected shortfall (cVaR)
