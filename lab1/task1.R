data <- read.csv("data.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)

data$ДатаРождения <- as.Date(data$ДатаРождения, format="%d.%m.%Y")
data$ДатаПервойПодачиЗаявления <- as.Date(data$ДатаПервойПодачиЗаявления, format="%d.%m.%Y")

data$Возраст <- as.numeric(difftime(data$ДатаПервойПодачиЗаявления, data$ДатаРождения, units = "days")) / 365

data[is.na(data)] <- 0
data$СуммаБаллов <- data$Оценка1 + data$Оценка2 + data$Оценка3

cairo_pdf("lab1.pdf", width = 8, height = 6, family = "DejaVu Serif", onefile =TRUE)

hist(
    data$СуммаБаллов, 
    main="Распределение суммы баллов", 
    xlab="Сумма баллов", 
    col="lightblue", 
    border="black"
    )

plot(
    data$Возраст,
    data$СуммаБаллов,  
    main="Сумма баллов vs Возраст", 
    xlab="Возраст", 
    ylab="Сумма баллов", 
    pch=19, 
    col="blue"
    )

boxplot(
    СуммаБаллов ~ substr(Страна, 1, 3), 
    data=data, 
    main="Сумма баллов по странам", 
    xlab="Страна", 
    ylab="Сумма баллов",
    las = 2
    )

boxplot(
    СуммаБаллов ~ УровеньПодготовки, 
    data=data, 
    main="Сумма баллов по уровню подготовки", 
    xlab="Уровень подготовки", 
    ylab="Сумма баллов"
    )

boxplot(
    СуммаБаллов ~ ОснованиеПоступления, 
    data=data, 
    main="Сумма баллов по форме обучения", 
    xlab="Форма обучения", 
    ylab="Сумма баллов"
    )

boxplot(
    СуммаБаллов ~ substr(СпециальностьНаименование, 1, 10), 
    data=data, 
    main="Сумма баллов по направлениям подготовки", 
    xlab="Направление подготовки", 
    ylab="Сумма баллов", 
    las=2, 
    cex.axis=0.5
    )

dev.off()

get_mean <- function(filtered_data) {
    mean_value <- sum(filtered_data$Оценка1 + filtered_data$Оценка2 + filtered_data$Оценка3, na.rm = TRUE) / nrow(filtered_data)
    return (mean_value)
}

get_median <- function(filtered_data) {
    sorted_data <- filtered_data[order(filtered_data$Оценка1 + filtered_data$Оценка2 + filtered_data$Оценка3), ]
    if(nrow(sorted_data) %% 2 == 1) {
        median_ <- sorted_data[nrow(sorted_data) / 2 + 1, ]
        return(median_$Оценка1 + median_$Оценка2 + median_$Оценка3)
    } else {
        median_1 <- sorted_data[nrow(sorted_data) / 2, ]
        median_2 <- sorted_data[nrow(sorted_data) / 2 + 1, ]
        return((median_1$Оценка1 + median_1$Оценка2 + median_1$Оценка3 + median_2$Оценка1 + median_2$Оценка2 + median_2$Оценка3) / 2)
    }
}

get_mode <- function(filtered_data) {
  unique_values <- unique(filtered_data)
  freq <- tabulate(match(filtered_data, unique_values))

  return(unique_values[freq == max(freq)])
}

get_variance <- function(filtered_data) {
  freq_table <- table(filtered_data)

  prob_table <- prop.table(freq_table)

  unique_values <- unique(filtered_data)

  M_x <- sum(unique_values * prob_table) # вот тут я вообще в шоке - он сам перемножит всё и суммирует

  M_x2 <- sum((unique_values^2) * prob_table)

  return (M_x2 - M_x^2) 
}

countries <- unique(na.omit(data$Страна))

for (country in countries) {
  cat('\n',"=======", country, "=======", '\n')
  cat("Среднее", ": ", get_mean(data[data$Страна == country, ]), "\n")

  cat("Медиана", ": ", get_median(data[data$Страна == country, ]), "\n")
                         
  cat("Мода", ": ", get_mode(data[data$Страна == country, "СуммаБаллов"]), "\n")

  cat("Дисперсия", ": ", get_variance(data[data$Страна == country, "СуммаБаллов"]), "\n")

  cat("СКO", ": ", get_variance(data[data$Страна == country, "СуммаБаллов"])^(0.5), "\n")
}