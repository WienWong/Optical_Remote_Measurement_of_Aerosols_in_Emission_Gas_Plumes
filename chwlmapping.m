function [S,P] = chwlmapping(CH,WL,k)
% 2016-11-30, 1st built, 2016-12-15, 2nd built, Wang Weihua
% Find and plot the channel wavelength mapping function by polynomial fitting.
% CH stands for channel. WL stands for wavelength. Both are vectors of same size.
% k is, the order of polynomial fitting, e.g. 1, 2, 3.
% Elements of S are the begin scope, end scope and dispersion of the 'Flame' spectrometer.
% CH = [273.87,  320.63,  388.83,  476.88,  546.50,  799.50];
% WL = [313.1844,334.1484,365.0158,404.6565,435.8335,546.0750];

p1=0;
p2=0;
p3=0;
p4=0;
p5=0;
numCols=2048;
S=[0 0 0];
dispersion=[0 0 0];
xx = 0:0.5:(numCols+2);  % xlim within [0, 2050] 
yy = zeros(size(xx));
figure;
scatter(CH,WL);
title('Channel-Wavelength mapping function');
xlabel('Channels');
ylabel('Wavelengths (nm)');
hold on;

if k==1
  p1 = polyfit(CH,WL,1);
  yy = p1(1)*xx + p1(2);
  plot(xx,yy,'r');
  xlim([0,2050]);
  ylim([0,1300]);
  legend('Channel-Wavelength pairs','Linear fitting');
  legend("location", "northwest");
  S(1) = p1(1)*1 + p1(2);       % wavelength corresponding to channel 1
  S(2) = p1(1)*numCols + p1(2); % wavelength corresponding to channel 2048
  P = [p1(1) p1(2)];
elseif k==2
  p2 = polyfit(CH,WL,2);
  yy = p2(1)*xx.^2 + p2(2)*xx + p2(3);
  plot(xx,yy,'r');
  xlim([0,2050]);
  ylim([0,1300]);
  legend('Channel-Wavelength pairs','2nd order polynomial fitting');
  legend("location", "northwest");
  S(1) = p2(1)*1^2 + p2(2)*1 + p2(3);
  S(2) = p2(1)*numCols^2 + p2(2)*numCols + p2(3);
  P = [p2(1) p2(2) p2(3)];
elseif k==3
  p3 = polyfit(CH,WL,3);
  yy = p3(1)*xx.^3 + p3(2)*xx.^2 + p3(3)*xx + p3(4);
  plot(xx,yy,'r');
  xlim([0,2050]);
  ylim([0,1300]);
  legend('Channel-Wavelength pairs','3rd order polynomial fitting');
  legend("location", "northwest");
  S(1) = p3(1)*1^3 + p3(2)*1^2 + p3(3)*1 + p3(4);
  S(2) = p3(1)*numCols^3 + p3(2)*numCols^2 + p3(3)*numCols + p3(4);
  P = [p3(1) p3(2) p3(3) p3(4)];
elseif k==4
  p4 = polyfit(CH,WL,4);
  yy = p4(1)*xx.^4 + p4(2)*xx.^3 + p4(3)*xx.^2 + p4(4)*xx + p4(5);
  plot(xx,yy,'r');
  xlim([0,2050]);
  ylim([0,1300]);
  legend('Channel-Wavelength pairs','4th order polynomial fitting');
  legend("location", "northwest");
  S(1) = p4(1)*1^4 + p4(2)*1^3 + p4(3)*1^2 + p4(4)*1 + p4(5);
  S(2) = p4(1)*numCols^4 + p4(2)*numCols^3 + p4(3)*numCols^2 + p4(4)*numCols + p4(5);
  P = [p4(1) p4(2) p4(3) p4(4) p4(5)];
elseif k==5  
  p5 = polyfit(CH,WL,5);
  yy = p5(1)*xx.^5 + p5(2)*xx.^4 + p5(3)*xx.^3 + p5(4)*xx.^2 + p5(5)*xx + p5(6);
  plot(xx,yy,'r');
  xlim([0,2050]);
  ylim([0,1300]);
  legend('Channel-Wavelength pairs','5th order polynomial fitting');
  legend("location", "northwest");
  S(1) = p5(1)*1^5 + p5(2)*1^4 + p5(3)*1^3 + p5(4)*1^2 + p5(5)*1 + p5(6);
  S(2) = p5(1)*numCols^5 + p5(2)*numCols^4 + p5(3)*numCols^3 + p5(4)*numCols^2 + p5(5)*numCols + p5(6);
  P = [p5(1) p5(2) p5(3) p5(4) p5(5) p5(6)];
else 
  disp('k = 1 or 2 or 3 or 4 or 5');
endif

dispersion = [(WL(2)-WL(1))/(CH(2)-CH(1)) (WL(4)-WL(3))/(CH(4)-CH(3)) (WL(6)-WL(5))/(CH(6)-CH(5)) (WL(6)-WL(1))/(CH(6)-CH(1)) ]
S(3) = mean(dispersion);

S = [S(1) S(2) S(3)];
P = P;
