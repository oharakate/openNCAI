# openNCAI #

openNCAI is an R package for calculating a regional natural capital assets index (NCAI), using the method designed by NatureScot to calculate <a href = "https://www.nature.scot/professional-advice/social-and-economic-benefits-nature/natural-capital/scotlands-natural-capital-asset-index">Scotland's NCAI</a>. It uses habitat extent and condition data, along with metadata and systems of weights, and produces a yearly single figure indexed around a year one value of 100.

The calculation of the NCAI is based on the following core measureable quantities and concepts:

| Concept | Explanation |
| :--- | :--- |
| **Natural habitat** | Natural habitats are areas where flora and fauna live. In economic terms, measurements of the **extent** (how much) and **condition** (how healthy) of natural habitats represent **stock**. |
| **Ecosystem service** | Functions performed by natural habitats which support our ecosystem. These might be providing food or fuel, cleaning the air, contributing to our culture. The index estimates **flow** of ecosystem services from our **stock** of natural capital, weighting by **importance** to our economy and community. |
| **Potential** | The notional capacity of a natural habitat to provide an ecosystem service. E.g., one hectare of market garden has the potential to provide more food than one hectare of cultivated garden in a public park. |
| **Importance** | The relative value of any ecosystem service to our community and economy. E.g. having drinking water is likely to be more essential to survival and well-being than rearing animals to pull carts and ploughs. |
| **Flow** | The actual rate of delivery of ecosystem services from a natural habitat. E.g. while a cornfield has good potential to contribute to the service of providing food from plants, the flow of that service could be reduced if poor weather meant the crop did not grow well. In calculating NCAI, data from a variety of sources are used as indicators of the condition of habitats and the subsequent likely flow of ecosystem services. |

We use the method designed by NatureScot for calculating Scotland’s NCAI (add refs), which relies on three types of information. These are:

1. Environmental measurements of the extent and condition of natural habitats – updated yearly (as available),
2. Lists of habitats in Scotland and the ecosystem services they provide – decided by expert opinion at the beginning of the process,
3. Weighting schemes which denote importance of ecosystem services, the potential of habitats to provide them, and the salience of habitat condition indicators to represent flow of services from habitats – decided by expert opinion at the beginning of the process.

