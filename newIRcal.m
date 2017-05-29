
function IRF = newIRcal(dp, Fstar, fileC)

% New way of calculating Intensity Ratio of the selected pairs, e.g. -ln(I340/I440)
% dp: dispersion
% Fstar: beginning wavelength of the 'Flame' spectrometer
% fileC: calibrated spectrum
% IRF: final calculated IRs.
% 2017-01-06 1st built, 2017-04-24, 2nd visit (add OnlyRatio), Weihua Wang

% Channel numbers regarding 310, 315, 340, 500, 675, 870 nm.
CH_310 = round( (310-Fstar) / dp );  %channel 264
%CH_311 = round( (311-Fstar) / dp ); %310-315 are SO2 active channels
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
I_CH_310 = fileC(:,CH_310);          %Contains all 2048 Intensities at the channel of 310 nm.
I_CH_340 = fileC(:,CH_340);
I_CH_380 = fileC(:,CH_380);
I_CH_440 = fileC(:,CH_440);
I_CH_500 = fileC(:,CH_500);
I_CH_675 = fileC(:,CH_675);
I_CH_870 = fileC(:,CH_870);

%% Intensity Ratio calculation using -log
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

%%% Intensity Ratio calculation using just ratio (Suggest by Johan)
%IR_310T340 = (I_CH_310 ./ I_CH_340);
%IR_340T440 = (I_CH_340 ./ I_CH_440);
%IR_340T500 = (I_CH_340 ./ I_CH_500);
%IR_340T675 = (I_CH_340 ./ I_CH_675);
%IR_340T870 = (I_CH_340 ./ I_CH_870);
%IR_440T675 = (I_CH_440 ./ I_CH_675);
%IR_440T500 = (I_CH_440 ./ I_CH_500);
%IR_440T870 = (I_CH_440 ./ I_CH_870);
%IR_500T675 = (I_CH_500 ./ I_CH_675);
%IR_500T870 = (I_CH_500 ./ I_CH_870);
%IR_675T870 = (I_CH_675 ./ I_CH_870);


IRF = [IR_310T340, IR_340T440, IR_340T500, IR_340T675, IR_340T870, IR_440T500,...
IR_440T675, IR_440T870, IR_500T675, IR_500T870, IR_675T870, I_CH_500];

endfunction
