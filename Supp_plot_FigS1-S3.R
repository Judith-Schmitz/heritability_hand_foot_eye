# Histograms: Figures S1-S3

### S1: HANDEDNESS

plot.s1 <- master %>%
  filter(!is.na(hand) & !is.na(m.hand) & !is.na(p.hand)) %>%
  select(ID_1, hand, hand.mean, m.hand, m.hand.mean, p.hand, p.hand.mean)

FigS1a <- ggplot(data = plot.s1, 
                 aes(x = hand.mean)) + 
  geom_histogram(binwidth = 0.05, color = "black", fill = "gray") +
  xlab("mean handedness score (children)") + 
  ylab("count") +
  theme_bw() + 
  theme(plot.title = element_text(),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold")) + 
  # Lines
  geom_vline(xintercept = 1.475, colour = "red", linetype = "longdash") + 
  geom_vline(xintercept = 2.625, colour = "red", linetype = "longdash") + 
  geom_vline(xintercept = 2.025, linetype = "longdash", colour = "blue") + 
  # Rectangles
  geom_rect(aes(xmin = 1, xmax = 1.475, ymin = 2800, ymax = 3500), fill = "white", color = "red", size = 1) + 
  geom_rect(aes(xmin = 1.475, xmax = 2.625, ymin = 2800, ymax = 3500), fill = "white", color = "red", size = 1) + 
  geom_rect(aes(xmin = 2.625, xmax = 3, ymin = 2800, ymax = 3500), fill = "white", color = "red", size = 1) + 
  geom_rect(aes(xmin = 1, xmax = 2.025, ymin = 2100, ymax = 2800), fill = "white", color = "blue", size = 1) + 
  geom_rect(aes(xmin = 2.025, xmax = 3, ymin = 2100, ymax = 2800), fill = "white", color = "blue", size = 1) + 
  # Text
  annotate("text", x = 1.2375, y = 3150, label = "n left = 389 \n (7.7%)", colour = "red") + 
  annotate("text", x = 2.05, y = 3150, label = "n mixed = 780 \n (15.5%)", colour = "red") + 
  annotate("text", x = 2.8125, y = 3150, label = "n right = 3859 \n (76.7%)", colour = "red") + 
  annotate("text", x = 1.5, y = 2450, label = "n left = 677 \n (13.5%)", colour = "blue") + 
  annotate("text", x = 2.5, y = 2450, label = "n right = 4351 \n (86.5%)", colour = "blue")

FigS1b <- ggplot(data = plot.s1, 
                 aes(x = m.hand.mean)) + 
  geom_histogram(binwidth = 0.05, color = "black", fill = "gray") +
  xlab("mean handedness score (mothers)") + 
  ylab("count") +
  theme_bw() + 
  theme(plot.title = element_text(),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold")) + 
  # Lines
  geom_vline(xintercept = 1.475, colour = "red", linetype = "longdash") + 
  geom_vline(xintercept = 2.625, colour = "red", linetype = "longdash") + 
  geom_vline(xintercept = 2.025, linetype = "longdash", colour = "blue") + 
  # Rectangles
  geom_rect(aes(xmin = 1, xmax = 1.475, ymin = 2800, ymax = 3500), fill = "white", color = "red", size = 1) + 
  geom_rect(aes(xmin = 1.475, xmax = 2.625, ymin = 2800, ymax = 3500), fill = "white", color = "red", size = 1) + 
  geom_rect(aes(xmin = 2.625, xmax = 3, ymin = 2800, ymax = 3500), fill = "white", color = "red", size = 1) + 
  geom_rect(aes(xmin = 1, xmax = 2.025, ymin = 2100, ymax = 2800), fill = "white", color = "blue", size = 1) + 
  geom_rect(aes(xmin = 2.025, xmax = 3, ymin = 2100, ymax = 2800), fill = "white", color = "blue", size = 1) + 
  # Text
  annotate("text", x = 1.2375, y = 3150, label = "n left = 383 \n (7.6%)", colour = "red") + 
  annotate("text", x = 2.05, y = 3150, label = "n mixed = 192 \n (3.8%)", colour = "red") + 
  annotate("text", x = 2.8125, y = 3150, label = "n right = 4453 \n (88.6%)", colour = "red") + 
  annotate("text", x = 1.5, y = 2450, label = "n left = 465 \n (9.2%)", colour = "blue") + 
  annotate("text", x = 2.5, y = 2450, label = "n right = 4563 \n (90.8%)", colour = "blue")

FigS1c <- ggplot(data = plot.s1, 
                 aes(x = p.hand.mean)) + 
  geom_histogram(binwidth = 0.05, color = "black", fill = "gray") +
  xlab("mean handedness score (fathers)") + 
  ylab("count") +
  theme_bw() + 
  theme(plot.title = element_text(),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold")) + 
  # Lines
  geom_vline(xintercept = 1.475, colour = "red", linetype = "longdash") + 
  geom_vline(xintercept = 2.625, colour = "red", linetype = "longdash") + 
  geom_vline(xintercept = 2.025, linetype = "longdash", colour = "blue") + 
  # Rectangles
  geom_rect(aes(xmin = 1, xmax = 1.475, ymin = 2800, ymax = 3500), fill = "white", color = "red", size = 1) + 
  geom_rect(aes(xmin = 1.475, xmax = 2.625, ymin = 2800, ymax = 3500), fill = "white", color = "red", size = 1) + 
  geom_rect(aes(xmin = 2.625, xmax = 3, ymin = 2800, ymax = 3500), fill = "white", color = "red", size = 1) + 
  geom_rect(aes(xmin = 1, xmax = 2.025, ymin = 2100, ymax = 2800), fill = "white", color = "blue", size = 1) + 
  geom_rect(aes(xmin = 2.025, xmax = 3, ymin = 2100, ymax = 2800), fill = "white", color = "blue", size = 1) + 
  # Text
  annotate("text", x = 1.2375, y = 3150, label = "n left = 420 \n (8.4%)", colour = "red") + 
  annotate("text", x = 2.05, y = 3150, label = "n mixed = 284 \n (5.6%)", colour = "red") + 
  annotate("text", x = 2.8125, y = 3150, label = "n right = 4324 \n (68.0%)", colour = "red") + 
  annotate("text", x = 1.5, y = 2450, label = "n left = 533 \n (10.6%)", colour = "blue") + 
  annotate("text", x = 2.5, y = 2450, label = "n right = 4495 \n (89.4%)", colour = "blue")

tiff("outputs/plots/FigS1_new.tiff", units = "in", width = 9, height = 12, res = 300)
ggarrange(FigS1a, FigS1b, FigS1c,
          labels = c("A", "B", "C"),
          ncol = 1, nrow = 3)
dev.off()


### S2: FOOTEDNESS

plot.s2 <- master %>%
  filter(!is.na(foot) & !is.na(m.foot) & !is.na(p.foot)) %>%
  select(ID_1, foot, foot.mean, m.foot, m.foot.mean, p.foot, p.foot.mean)

FigS2a <- ggplot(data = plot.s2, 
                 aes(x = foot.mean)) + 
  geom_histogram(binwidth = 0.05, color = "black", fill = "gray") +
  xlab("mean footedness score (children)") + 
  ylab("count") +
  theme_bw() + 
  theme(plot.title = element_text(),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold")) + 
  # Lines
  geom_vline(xintercept = 1.475, colour = "red", linetype = "longdash") + 
  geom_vline(xintercept = 2.625, colour = "red", linetype = "longdash") + 
  geom_vline(xintercept = 2.025, linetype = "longdash", colour = "blue") + 
  # Rectangles
  geom_rect(aes(xmin = 1, xmax = 1.475, ymin = 2800, ymax = 3500), fill = "white", color = "red", size = 1) + 
  geom_rect(aes(xmin = 1.475, xmax = 2.625, ymin = 2800, ymax = 3500), fill = "white", color = "red", size = 1) + 
  geom_rect(aes(xmin = 2.625, xmax = 3, ymin = 2800, ymax = 3500), fill = "white", color = "red", size = 1) + 
  geom_rect(aes(xmin = 1, xmax = 2.025, ymin = 2100, ymax = 2800), fill = "white", color = "blue", size = 1) + 
  geom_rect(aes(xmin = 2.025, xmax = 3, ymin = 2100, ymax = 2800), fill = "white", color = "blue", size = 1) + 
  # Text
  annotate("text", x = 1.2375, y = 3150, label = "n left = 285 \n (5.7%)", colour = "red") + 
  annotate("text", x = 2.05, y = 3150, label = "n mixed = 1718 \n (34.6%)", colour = "red") + 
  annotate("text", x = 2.8125, y = 3150, label = "n right = 2957 \n (59.6%)", colour = "red") + 
  annotate("text", x = 1.5, y = 2450, label = "n left = 913 \n (18.4%)", colour = "blue") + 
  annotate("text", x = 2.5, y = 2450, label = "n right = 4047 \n (81.6)", colour = "blue")

FigS2b <- ggplot(data = plot.s2, 
                 aes(x = m.foot.mean)) + 
  geom_histogram(binwidth = 0.05, color = "black", fill = "gray") +
  xlab("mean footedness score (mothers)") + 
  ylab("count") +
  theme_bw() + 
  theme(plot.title = element_text(),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold")) + 
  # Lines
  geom_vline(xintercept = 1.475, colour = "red", linetype = "longdash") + 
  geom_vline(xintercept = 2.625, colour = "red", linetype = "longdash") + 
  geom_vline(xintercept = 2.025, linetype = "longdash", colour = "blue") + 
  # Rectangles
  geom_rect(aes(xmin = 1, xmax = 1.475, ymin = 2800, ymax = 3500), fill = "white", color = "red", size = 1) + 
  geom_rect(aes(xmin = 1.475, xmax = 2.625, ymin = 2800, ymax = 3500), fill = "white", color = "red", size = 1) + 
  geom_rect(aes(xmin = 2.625, xmax = 3, ymin = 2800, ymax = 3500), fill = "white", color = "red", size = 1) + 
  geom_rect(aes(xmin = 1, xmax = 2.025, ymin = 2100, ymax = 2800), fill = "white", color = "blue", size = 1) + 
  geom_rect(aes(xmin = 2.025, xmax = 3, ymin = 2100, ymax = 2800), fill = "white", color = "blue", size = 1) + 
  # Text
  annotate("text", x = 1.2375, y = 3150, label = "n left = 234 \n (4.7%)", colour = "red") + 
  annotate("text", x = 2.05, y = 3150, label = "n mixed = 1536 \n (31.0%)", colour = "red") + 
  annotate("text", x = 2.8125, y = 3150, label = "n right = 3190 \n (64.3%)", colour = "red") + 
  annotate("text", x = 1.5, y = 2450, label = "n left = 602 \n (12.1%)", colour = "blue") + 
  annotate("text", x = 2.5, y = 2450, label = "n right = 4358 \n (87.9%)", colour = "blue")

FigS2c <- ggplot(data = plot.s2, 
                 aes(x = p.foot.mean)) + 
  geom_histogram(binwidth = 0.05, color = "black", fill = "gray") +
  xlab("mean footedness score (fathers)") + 
  ylab("count") +
  theme_bw() + 
  theme(plot.title = element_text(),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold")) + 
  # Lines
  geom_vline(xintercept = 1.475, colour = "red", linetype = "longdash") + 
  geom_vline(xintercept = 2.625, colour = "red", linetype = "longdash") + 
  geom_vline(xintercept = 2.025, linetype = "longdash", colour = "blue") + 
  # Rectangles
  geom_rect(aes(xmin = 1, xmax = 1.475, ymin = 2800, ymax = 3500), fill = "white", color = "red", size = 1) + 
  geom_rect(aes(xmin = 1.475, xmax = 2.625, ymin = 2800, ymax = 3500), fill = "white", color = "red", size = 1) + 
  geom_rect(aes(xmin = 2.625, xmax = 3, ymin = 2800, ymax = 3500), fill = "white", color = "red", size = 1) + 
  geom_rect(aes(xmin = 1, xmax = 2.025, ymin = 2100, ymax = 2800), fill = "white", color = "blue", size = 1) + 
  geom_rect(aes(xmin = 2.025, xmax = 3, ymin = 2100, ymax = 2800), fill = "white", color = "blue", size = 1) + 
  # Text
  annotate("text", x = 1.2375, y = 3150, label = "n left = 274 \n (5.5%)", colour = "red") + 
  annotate("text", x = 2.05, y = 3150, label = "n mixed = 2369 \n (47.8%)", colour = "red") + 
  annotate("text", x = 2.8125, y = 3150, label = "n right = 2317 \n (46.7%)", colour = "red") + 
  annotate("text", x = 1.5, y = 2450, label = "n left = 1064 \n (21.5%)", colour = "blue") + 
  annotate("text", x = 2.5, y = 2450, label = "n right = 3896 \n (78.5%)", colour = "blue")

tiff("outputs/plots/FigS2_new.tiff", units = "in", width = 9, height = 12, res = 300)
ggarrange(FigS2a, FigS2b, FigS2c,
          labels = c("A", "B", "C"),
          ncol = 1, nrow = 3)
dev.off()


### S3: EYEDNESS

plot.s3 <- master %>%
  filter(!is.na(eye) & !is.na(m.eye) & !is.na(p.eye)) %>%
  select(ID_1, eye, eye.mean, m.eye, m.eye.mean, p.eye, p.eye.mean)

FigS3a <- ggplot(data = plot.s3, 
                 aes(x = eye.mean)) + 
  geom_histogram(binwidth = 0.05, color = "black", fill = "gray") +
  xlab("mean eyedness score (children)") + 
  ylab("count") +
  theme_bw() + 
  theme(plot.title = element_text(),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold")) + 
  # Lines
  geom_vline(xintercept = 1.475, colour = "red", linetype = "longdash") + 
  geom_vline(xintercept = 2.625, colour = "red", linetype = "longdash") + 
  geom_vline(xintercept = 2.025, linetype = "longdash", colour = "blue") + 
  # Rectangles
  geom_rect(aes(xmin = 1, xmax = 1.475, ymin = 2800, ymax = 3500), fill = "white", color = "red", size = 1) + 
  geom_rect(aes(xmin = 1.475, xmax = 2.625, ymin = 2800, ymax = 3500), fill = "white", color = "red", size = 1) + 
  geom_rect(aes(xmin = 2.625, xmax = 3, ymin = 2800, ymax = 3500), fill = "white", color = "red", size = 1) + 
  geom_rect(aes(xmin = 1, xmax = 2.025, ymin = 2100, ymax = 2800), fill = "white", color = "blue", size = 1) + 
  geom_rect(aes(xmin = 2.025, xmax = 3, ymin = 2100, ymax = 2800), fill = "white", color = "blue", size = 1) + 
  # Text
  annotate("text", x = 1.2375, y = 3150, label = "n left = 608 \n (12.8%)", colour = "red") + 
  annotate("text", x = 2.05, y = 3150, label = "n mixed = 1712 \n (36.0%)", colour = "red") + 
  annotate("text", x = 2.8125, y = 3150, label = "n right = 2442 \n (51.3%)", colour = "red") + 
  annotate("text", x = 1.5, y = 2450, label = "n left = 2037 \n (42.8%)", colour = "blue") + 
  annotate("text", x = 2.5, y = 2450, label = "n right = 2725 \n (57.2%)", colour = "blue")

FigS3b <- ggplot(data = plot.s3, 
                 aes(x = m.eye.mean)) + 
  geom_histogram(binwidth = 0.05, color = "black", fill = "gray") +
  xlab("mean eyedness score (mothers)") + 
  ylab("count") +
  theme_bw() + 
  theme(plot.title = element_text(),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold")) + 
  # Lines
  geom_vline(xintercept = 1.475, colour = "red", linetype = "longdash") + 
  geom_vline(xintercept = 2.625, colour = "red", linetype = "longdash") + 
  geom_vline(xintercept = 2.025, linetype = "longdash", colour = "blue") + 
  # Rectangles
  geom_rect(aes(xmin = 1, xmax = 1.475, ymin = 2800, ymax = 3500), fill = "white", color = "red", size = 1) + 
  geom_rect(aes(xmin = 1.475, xmax = 2.625, ymin = 2800, ymax = 3500), fill = "white", color = "red", size = 1) + 
  geom_rect(aes(xmin = 2.625, xmax = 3, ymin = 2800, ymax = 3500), fill = "white", color = "red", size = 1) + 
  geom_rect(aes(xmin = 1, xmax = 2.025, ymin = 2100, ymax = 2800), fill = "white", color = "blue", size = 1) + 
  geom_rect(aes(xmin = 2.025, xmax = 3, ymin = 2100, ymax = 2800), fill = "white", color = "blue", size = 1) + 
  # Text
  annotate("text", x = 1.2375, y = 3150, label = "n left = 847 \n (17.8%)", colour = "red") + 
  annotate("text", x = 2.05, y = 3150, label = "n mixed = 837 \n (17.6%)", colour = "red") + 
  annotate("text", x = 2.8125, y = 3150, label = "n right = 3078 \n (64.6%)", colour = "red") + 
  annotate("text", x = 1.5, y = 2450, label = "n left = 1446 \n (30.4%)", colour = "blue") + 
  annotate("text", x = 2.5, y = 2450, label = "n right = 3316 \n (69.6%)", colour = "blue")

FigS3c <- ggplot(data = plot.s3, 
                 aes(x = p.eye.mean)) + 
  geom_histogram(binwidth = 0.05, color = "black", fill = "gray") +
  xlab("mean eyedness score (fathers)") + 
  ylab("count") +
  theme_bw() + 
  theme(plot.title = element_text(),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold")) + 
  # Lines
  geom_vline(xintercept = 1.475, colour = "red", linetype = "longdash") + 
  geom_vline(xintercept = 2.625, colour = "red", linetype = "longdash") + 
  geom_vline(xintercept = 2.025, linetype = "longdash", colour = "blue") + 
  # Rectangles
  geom_rect(aes(xmin = 1, xmax = 1.475, ymin = 2800, ymax = 3500), fill = "white", color = "red", size = 1) + 
  geom_rect(aes(xmin = 1.475, xmax = 2.625, ymin = 2800, ymax = 3500), fill = "white", color = "red", size = 1) + 
  geom_rect(aes(xmin = 2.625, xmax = 3, ymin = 2800, ymax = 3500), fill = "white", color = "red", size = 1) + 
  geom_rect(aes(xmin = 1, xmax = 2.025, ymin = 2100, ymax = 2800), fill = "white", color = "blue", size = 1) + 
  geom_rect(aes(xmin = 2.025, xmax = 3, ymin = 2100, ymax = 2800), fill = "white", color = "blue", size = 1) + 
  # Text
  annotate("text", x = 1.2375, y = 3150, label = "n left = 734 \n (15.4%)", colour = "red") + 
  annotate("text", x = 2.05, y = 3150, label = "n mixed = 1041 \n (21.9%)", colour = "red") + 
  annotate("text", x = 2.8125, y = 3150, label = "n right = 2987 \n (62.7%)", colour = "red") + 
  annotate("text", x = 1.5, y = 2450, label = "n left = 1445 \n (30.3%)", colour = "blue") + 
  annotate("text", x = 2.5, y = 2450, label = "n right = 3317 \n (69.7%)", colour = "blue")

tiff("outputs/plots/FigS3_new.tiff", units = "in", width = 9, height = 12, res = 300)
ggarrange(FigS3a, FigS3b, FigS3c,
          labels = c("A", "B", "C"),
          ncol = 1, nrow = 3)
dev.off()



















