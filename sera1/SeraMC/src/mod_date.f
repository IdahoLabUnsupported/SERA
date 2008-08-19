      SUBROUTINE mod_date                                                       
                                                                                
      WRITE(6,101)                                                              
  101 FORMAT(/,5X,'Version 1C0 - Last modified Tue. 19 Oct., 2004')
  102 FORMAT(/,4X,'routine  version ',/                          
     *,5X,'DoseOpt.f#3',/
     *,5X,'Tlook.f#3',/
     *,5X,'cg.f#1',/
     *,5X,'epmc.f#6',/
     *,5X,'fund.f#2',/
     *,5X,'gampro.f#4',/
     *,5X,'libin.f#5',/
     *,5X,'locate.f#6',/
     *,5X,'proptd.f#4',/
     *,5X,'rand.f#1',/
     *,5X,'rst_edit.f#12',/
     *,5X,'rtt_MC.f#10',/
     *,5X,'scatter.f#2',/
     *,5X,'source.f#3',/
     *,5X,'track.f#6',/
     *,5X,'tyme_hp.f#2',/
     *,5X,'ultraxs.f#1',/
     *,5X,'volume.f#9',/
     *,5X,'cputime.c#2',/
     *,5X,'envr.c#1',/
     *,5X,'image.c#1',/
     *,5X,'read_row.c#1',/
     *,5X,'write_row.c#1,')
  103 FORMAT(/,5X,'Compiled on host: dragon                  ',/)                 
                                                                                
      RETURN                                                                    
      END                                                                       
