
library(readxl)
library(ggplot2)
library(dplyr)
library(MASS)

# Import data nakes
df <- read_excel('Data Penelitian Mahasiswa All Nakes edit.xlsx')
# Menghapus baris dengan nilai NA menggunakan drop_na()
df_clean <- na.omit(df)
numeric_data <- df_clean[, 4:14]

# Hitung matriks jarak Euclidean
d <- dist(as.matrix(df_clean, method = "euclidean"))
print(d)

# Menghitung nilai dan vektor eigen
A <- d
eigen_result <- eigen(A)
eigen_result

eigen_values <- eigen_result$valuess
eigen_vectors <- eigen_result$vectors

print(eigen_values)
print(eigen_vectors)

mds_result <- cmdscale(d, k=2)
# Convert the MDS result to a data frame
mds_df <- as.data.frame(mds_result)
colnames(mds_df) <- c("Dim1", "Dim2")
mds_df$kabupaten_kota <- df_clean$kabupaten_kota

# Nilai STRESS
stress <- sum((d - dist(mds_result))^2) / sum(d^2)
print(stress)

  # Contoh plot dengan label objek
p <- ggplot(mds_df, aes(x = Dim1, y = Dim2, label = kabupaten_kota)) +
  geom_point() +
  geom_text(size = 3, hjust = 0, vjust = 0) +  # Teks label
  labs(title = "Multidimensional Scaling Plot") +
  theme_minimal()

p + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue")
