# size 분석 (참고사항 - size변수 삭제하기 전에 사용해보기)

## size
size=xtabs(~Size + price_G,data=hyundai)
size=size[,c(2,1)] ; size

chisq.test(size) # size와 중고차 등급은 관련있음
chisq.test(size)$stdres

size = data.frame(Size=c("S","M","L"),"Yes"=c(104,1039,71),"No"=c(2026,1569,48)) ; size

n=size$Yes+size$No
fit = glm(size$Yes/n ~ Size, family = binomial, weights = n, data=size)
summary(fit) # Large 기준변수
exp(-0.8037) # Medium이 1에 속할 오즈는 Large 오즈의 0.43배
exp(-3.3609) # Small이 1에 속할 오즈는 Large 오즈의 0.033배
