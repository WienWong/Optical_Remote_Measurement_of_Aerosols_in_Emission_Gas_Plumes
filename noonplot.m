
% Noon plot of Intensity at 500 nm 
% 2017-01-04, 1st built, Wang Weihua

clear;

dat1 = load('I500_noon0508.dat'); 
dat2 = load('I500_noon0511.dat');
dat3 = load('I500_noon0513.dat');
dat4 = load('I500_noon0515.dat');
dat5 = load('I500_noon0516.dat');
dat6 = load('I500_noon0517.dat');

figure;
plot(dat1(1,:), dat1(2,:));
hold on;
plot(dat2(1,:), dat2(2,:), '-r');
plot(dat3(1,:), dat3(2,:), '-k');
plot(dat4(1,:), dat4(2,:), '-g');
plot(dat5(1,:), dat5(2,:), '-c');
plot(dat6(1,:), dat6(2,:), '-m');
legend('May08','May11','May13','May15','May16','May17');
title('I_{500} Noon Plot');
xlabel('Seconds');ylabel('Intensity');

figure;
subplot(231);plot(dat1(1,:), dat1(2,:));title('I_{500} Noon on May08');xlabel('Seconds');ylabel('Intensity');
subplot(232);plot(dat2(1,:), dat2(2,:));title('I_{500} Noon on May11');xlabel('Seconds');ylabel('Intensity');
subplot(233);plot(dat3(1,:), dat3(2,:));title('I_{500} Noon on May13');xlabel('Seconds');ylabel('Intensity');
subplot(234);plot(dat4(1,:), dat4(2,:));title('I_{500} Noon on May15');xlabel('Seconds');ylabel('Intensity');
subplot(235);plot(dat5(1,:), dat5(2,:));title('I_{500} Noon on May16');xlabel('Seconds');ylabel('Intensity');
subplot(236);plot(dat6(1,:), dat6(2,:));title('I_{500} Noon on May17');xlabel('Seconds');ylabel('Intensity');
