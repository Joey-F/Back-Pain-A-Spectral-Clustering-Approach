# Back-Pain-A-Spectral-Clustering-Approach
I use a Spectral Clustering algorithm to find clusters among medical
patients with lower back pain symptoms, and then I assess the health
outcomes within each cluster. First, I map all of the variables onto [0:1]
intervals. I use these standardized variables to compute a similarity score between every pair of
patients, using an adaptation of Pearson correlation (centered cosine similarity). I then calculate the
spectral (eigen) decomposition of this similarity matrix, and I use the first
few eigenvectors to create a low-dimensional subspace. Finally, I perform
k–means clustering in this new subspace to find four clusters. I compare the
cluster means and variances for each recovery assessment variable, highlighting
the distinct health outcomes for each cluster. Lastly, I identify the physical
symptoms of each patient cluster by inspecting any variable whose within–
cluster average is extraordinarily low or high, relative to the other clusters.
