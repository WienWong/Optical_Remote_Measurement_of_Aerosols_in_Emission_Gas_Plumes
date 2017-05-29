function IRF = newIRcal_Seg(dp, Fstar, fileC, ts, numfile)

% New way of calculating Intensity Ratio of the selected pairs with segmentation in time, e.g. -ln(I340/I440)
% dp: dispersion
% Fstar: beginning wavelength of the spectrometer
% fileC: calibrated spectrum
% numfile: how many STD files for each directory.
% ts: timeslot between two measurements.
% IRF: final calculated IRs.
% 2017-01-09 first built, Weihua Wang

% Channel numbers regarding 310, 315, 340, 500, 675, 870 nm.
CH_310 = round( (310-Fstar) / dp );  %channel 264
%CH_311 = round( (311-Fstar) / dp );
%CH_312 = round( (312-Fstar) / dp );
%CH_313 = round( (313-Fstar) / dp );
%CH_314 = round( (314-Fstar) / dp );
%CH_315 = round( (315-Fstar) / dp ); %channel 275
CH_340 = round( (340-Fstar) / dp );  %channel 331
CH_380 = round( (380-Fstar) / dp );  %channel 421
CH_440 = round( (440-Fstar) / dp );  %channel 556
CH_500 = round( (500-Fstar) / dp );  %channel 690
CH_675 = round( (675-Fstar) / dp );  %channel 1085
CH_870 = round( (870-Fstar) / dp );  %channel 1524

%
I_CH_310 = fileC(:,CH_310);          %Contains all 2048 Intensities at 310 nm.
I_CH_340 = fileC(:,CH_340);
I_CH_380 = fileC(:,CH_380);
I_CH_440 = fileC(:,CH_440);
I_CH_500 = fileC(:,CH_500);
I_CH_675 = fileC(:,CH_675);
I_CH_870 = fileC(:,CH_870);

% Intensity Ratio calculation
IR_310T340 = (-1)*log(I_CH_310 ./ I_CH_340);
IR_340T440 = (-1)*log(I_CH_340 ./ I_CH_440);
IR_340T500 = (-1)*log(I_CH_340 ./ I_CH_500);
IR_340T675 = (-1)*log(I_CH_340 ./ I_CH_675);
IR_340T870 = (-1)*log(I_CH_340 ./ I_CH_870);
IR_440T675 = (-1)*log(I_CH_440 ./ I_CH_675);
IR_440T500 = (-1)*log(I_CH_440 ./ I_CH_500);
IR_440T870 = (-1)*log(I_CH_440 ./ I_CH_870);
IR_500T675 = (-1)*log(I_CH_500 ./ I_CH_675);
IR_500T870 = (-1)*log(I_CH_500 ./ I_CH_870);
IR_675T870 = (-1)*log(I_CH_675 ./ I_CH_870);

if( (length(ts) == 1) && (length(numfile) == 2) )
for kk=(numfile(1) + 1):(numfile(1) + ts)
  IR_310T340(kk)=0;
  IR_315T340(kk)=0;
  IR_340T500(kk)=0;
  IR_340T675(kk)=0;
  IR_340T870(kk)=0;
  IR_440T675(kk)=0;
  IR_440T870(kk)=0;
  IR_500T870(kk)=0;
  I_CH_500(kk)=0;
  I_CH_340(kk)=0;
  I_CH_310(kk)=0;
  I_CH_315(kk)=0;  
  I_CH_380(kk)=0;
  I_CH_440(kk)=0;
  I_CH_675(kk)=0;
  I_CH_870(kk)=0; 
end
elseif( (length(ts) == 2) && (length(numfile) == 3) )
for kk=(numfile(1)+1):(numfile(1)+ts(1))
  IR_310T340(kk)=0;
  IR_315T340(kk)=0;
  IR_340T500(kk)=0;
  IR_340T675(kk)=0;
  IR_340T870(kk)=0;
  IR_440T675(kk)=0;
  IR_440T870(kk)=0;
  IR_500T870(kk)=0;
  I_CH_500(kk)=0;
  I_CH_340(kk)=0;
  I_CH_310(kk)=0;
  I_CH_315(kk)=0;  
  I_CH_380(kk)=0;
  I_CH_440(kk)=0;
  I_CH_675(kk)=0;
  I_CH_870(kk)=0; 
end
for kk=(numfile(2)+1):(numfile(2)+ts(2))
  IR_310T340(kk)=0;
  IR_315T340(kk)=0;
  IR_340T500(kk)=0;
  IR_340T675(kk)=0;
  IR_340T870(kk)=0;
  IR_440T675(kk)=0;
  IR_440T870(kk)=0;
  IR_500T870(kk)=0;
  I_CH_500(kk)=0;
  I_CH_340(kk)=0;
  I_CH_310(kk)=0;
  I_CH_315(kk)=0;  
  I_CH_380(kk)=0;
  I_CH_440(kk)=0;
  I_CH_675(kk)=0;
  I_CH_870(kk)=0; 
end

endif

IRF = [IR_310T340, IR_340T440, IR_340T500, IR_340T675, IR_340T870, IR_440T500,...
IR_440T675, IR_440T870, IR_500T675, IR_500T870, IR_675T870, I_CH_500];

endfunction

