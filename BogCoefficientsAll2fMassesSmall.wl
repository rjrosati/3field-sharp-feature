regIIIcoeffsSimp = {\[Alpha]zz -> 
      ((E^(I*\[Delta]*\[Omega]2)*(\[Kappa]^2 - \[Omega]1^2)*
          (I + \[Kappa] - \[Omega]2))/((-\[Omega]1 + \[Omega]2)*
          (\[Omega]1 + \[Omega]2)) + (E^(I*\[Delta]*\[Omega]1)*
          (I + \[Kappa] - \[Omega]1)*(\[Kappa]^2 - \[Omega]2^2))/
         ((\[Omega]1 - \[Omega]2)*(\[Omega]1 + \[Omega]2)) + 
        ((I + \[Kappa] + \[Omega]1)*(\[Kappa]^2 - \[Omega]2^2))/
         (E^(I*\[Delta]*\[Omega]1)*(\[Omega]1^2 - \[Omega]2^2)) + 
        ((\[Kappa]^2 - \[Omega]1^2)*(I + \[Kappa] + \[Omega]2))/
         (E^(I*\[Delta]*\[Omega]2)*(-\[Omega]1^2 + \[Omega]2^2)) + 
        (I*(-I + \[Kappa])*(E^(I*\[Delta]*(\[Omega]1 + 2*\[Omega]2))*
            \[Omega]1*(-\[Kappa]^2 + \[Omega]1^2)*(1 - I*\[Kappa] + 
             I*\[Omega]2) + I*E^(I*\[Delta]*\[Omega]1)*\[Omega]1*
            (-\[Kappa]^2 + \[Omega]1^2)*(I + \[Kappa] + \[Omega]2) - 
           I*E^(I*\[Delta]*(2*\[Omega]1 + \[Omega]2))*(-I - \[Kappa] + 
             \[Omega]1)*\[Omega]2*(-\[Kappa]^2 + \[Omega]2^2) - 
           I*E^(I*\[Delta]*\[Omega]2)*(I + \[Kappa] + \[Omega]1)*\[Omega]2*
            (-\[Kappa]^2 + \[Omega]2^2)))/
         (E^(I*\[Delta]*(\[Omega]1 + \[Omega]2))*\[Omega]1*\[Omega]2*
          (-\[Omega]1 + \[Omega]2)*(\[Omega]1 + \[Omega]2)))/(4*\[Kappa]), 
     \[Alpha]zs -> ((-1)^(1/4)*E^((I/4)*(Sqrt[9 - 4*mss0]*Pi - 
           4*(\[Kappa] + \[Delta]*(\[Omega]1 + \[Omega]2))))*Sqrt[Pi/2]*
        Sqrt[\[Kappa]]*\[CapitalOmega]0*
        ((-E^(I*\[Delta]*\[Omega]1) + E^(I*\[Delta]*\[Omega]2) + 
           E^(I*\[Delta]*(2*\[Omega]1 + \[Omega]2)) - 
           E^(I*\[Delta]*(\[Omega]1 + 2*\[Omega]2)))*\[Kappa]*
          HankelH1[-1 + Sqrt[9/4 - mss0], \[Kappa]] + 
         (E^(I*\[Delta]*\[Omega]1) - E^(I*\[Delta]*\[Omega]2) - 
           E^(I*\[Delta]*(2*\[Omega]1 + \[Omega]2)) + 
           E^(I*\[Delta]*(\[Omega]1 + 2*\[Omega]2)))*\[Kappa]*
          HankelH1[1 + Sqrt[9/4 - mss0], \[Kappa]] + 
         (E^(I*\[Delta]*(2*\[Omega]1 + \[Omega]2))*(3 - (2*I)*\[Omega]1) + 
           E^(I*\[Delta]*\[Omega]2)*(3 + (2*I)*\[Omega]1) + 
           E^(I*\[Delta]*\[Omega]1)*(-3 - (2*I)*\[Omega]2) + 
           E^(I*\[Delta]*(\[Omega]1 + 2*\[Omega]2))*(-3 + (2*I)*\[Omega]2))*
          HankelH1[Sqrt[9/4 - mss0], \[Kappa]] + 
         ((-I + \[Kappa])*(-(\[Kappa]*(E^(I*\[Delta]*\[Omega]1)*\[Omega]1 - 
               E^(I*\[Delta]*(\[Omega]1 + 2*\[Omega]2))*\[Omega]1 - 
               E^(I*\[Delta]*\[Omega]2)*\[Omega]2 + E^(I*\[Delta]*
                  (2*\[Omega]1 + \[Omega]2))*\[Omega]2)*HankelH1[-1 + 
                Sqrt[9/4 - mss0], \[Kappa]]) + \[Kappa]*
             (E^(I*\[Delta]*\[Omega]1)*\[Omega]1 - E^(I*\[Delta]*(\[Omega]1 + 
                  2*\[Omega]2))*\[Omega]1 - E^(I*\[Delta]*\[Omega]2)*
               \[Omega]2 + E^(I*\[Delta]*(2*\[Omega]1 + \[Omega]2))*
               \[Omega]2)*HankelH1[1 + Sqrt[9/4 - mss0], \[Kappa]] + 
            (E^(I*\[Delta]*\[Omega]1)*\[Omega]1*(-3 - (2*I)*\[Omega]2) + 
              E^(I*\[Delta]*(\[Omega]1 + 2*\[Omega]2))*\[Omega]1*(3 - 
                (2*I)*\[Omega]2) + E^(I*\[Delta]*\[Omega]2)*(3 + 
                (2*I)*\[Omega]1)*\[Omega]2 + I*E^(I*\[Delta]*(2*\[Omega]1 + 
                  \[Omega]2))*(3*I + 2*\[Omega]1)*\[Omega]2)*
             HankelH1[Sqrt[9/4 - mss0], \[Kappa]]))/(\[Omega]1*\[Omega]2)))/
       (4*(\[Omega]1^2 - \[Omega]2^2)), \[Alpha]sz -> 
      -1/2*(-2*((E^(I*\[Delta]*\[Omega]2)*(\[Kappa]^2 - \[Omega]1^2)*
             (\[Kappa] - \[Omega]2)*(I + \[Kappa] - \[Omega]2)*
             (\[Kappa] + \[Omega]2))/((-\[Omega]1 + \[Omega]2)*
             (\[Omega]1 + \[Omega]2)) + (E^(I*\[Delta]*\[Omega]1)*
             (\[Kappa] - \[Omega]1)*(I + \[Kappa] - \[Omega]1)*
             (\[Kappa] + \[Omega]1)*(\[Kappa]^2 - \[Omega]2^2))/
            ((\[Omega]1 - \[Omega]2)*(\[Omega]1 + \[Omega]2)) + 
           ((\[Kappa] - \[Omega]1)*(\[Kappa] + \[Omega]1)*(I + \[Kappa] + 
              \[Omega]1)*(\[Kappa]^2 - \[Omega]2^2))/
            (E^(I*\[Delta]*\[Omega]1)*(\[Omega]1^2 - \[Omega]2^2)) + 
           ((\[Kappa]^2 - \[Omega]1^2)*(\[Kappa] - \[Omega]2)*
             (\[Kappa] + \[Omega]2)*(I + \[Kappa] + \[Omega]2))/
            (E^(I*\[Delta]*\[Omega]2)*(-\[Omega]1^2 + \[Omega]2^2)))*
          HankelH2[Sqrt[9/4 - mss0], \[Kappa]] - 
         I*((E^(I*\[Delta]*\[Omega]2)*(\[Kappa]^2 - \[Omega]1^2)*
             (I + \[Kappa] - \[Omega]2)*(-\[Kappa] + \[Omega]2)*
             (\[Kappa] + \[Omega]2))/(\[Omega]2*(-\[Omega]1 + \[Omega]2)*
             (\[Omega]1 + \[Omega]2)) + (E^(I*\[Delta]*\[Omega]1)*
             (I + \[Kappa] - \[Omega]1)*(-\[Kappa] + \[Omega]1)*
             (\[Kappa] + \[Omega]1)*(\[Kappa]^2 - \[Omega]2^2))/
            (\[Omega]1*(\[Omega]1 - \[Omega]2)*(\[Omega]1 + \[Omega]2)) - 
           ((-\[Kappa] + \[Omega]1)*(\[Kappa] + \[Omega]1)*(I + \[Kappa] + 
              \[Omega]1)*(\[Kappa]^2 - \[Omega]2^2))/
            (E^(I*\[Delta]*\[Omega]1)*(\[Omega]1^3 - \[Omega]1*\[Omega]2^
                2)) + ((\[Kappa]^2 - \[Omega]1^2)*(-\[Kappa] + \[Omega]2)*
             (\[Kappa] + \[Omega]2)*(I + \[Kappa] + \[Omega]2))/
            (E^(I*\[Delta]*\[Omega]2)*(\[Omega]1^2*\[Omega]2 - \[Omega]2^3)))*
          (\[Kappa]*HankelH2[-1 + Sqrt[9/4 - mss0], \[Kappa]] - 
           \[Kappa]*HankelH2[1 + Sqrt[9/4 - mss0], \[Kappa]] + 
           3*HankelH2[Sqrt[9/4 - mss0], \[Kappa]]))/
        (E^((I/4)*(Pi + Sqrt[9 - 4*mss0]*Pi - 4*\[Kappa]))*Sqrt[2*Pi]*
         \[Kappa]^(5/2)*\[CapitalOmega]0*(HankelH1[Sqrt[9/4 - mss0], 
            \[Kappa]]*(HankelH2[-1 + Sqrt[9/4 - mss0], \[Kappa]] - 
            HankelH2[1 + Sqrt[9/4 - mss0], \[Kappa]]) + 
          (-HankelH1[-1 + Sqrt[9/4 - mss0], \[Kappa]] + 
            HankelH1[1 + Sqrt[9/4 - mss0], \[Kappa]])*
           HankelH2[Sqrt[9/4 - mss0], \[Kappa]])), 
     \[Alpha]ss -> -1/4*(-2*(E^(I*\[Delta]*\[Omega]1)*(\[Kappa] - \[Omega]1)*
            (\[Kappa] + \[Omega]1)*(\[Kappa]*HankelH1[-1 + Sqrt[9/4 - mss0], 
               \[Kappa]] - \[Kappa]*HankelH1[1 + Sqrt[9/4 - mss0], 
               \[Kappa]] + (3 - (2*I)*\[Omega]1)*HankelH1[Sqrt[9/4 - mss0], 
               \[Kappa]]) + ((\[Kappa] - \[Omega]1)*(\[Kappa] + \[Omega]1)*
             (\[Kappa]*HankelH1[-1 + Sqrt[9/4 - mss0], \[Kappa]] - 
              \[Kappa]*HankelH1[1 + Sqrt[9/4 - mss0], \[Kappa]] + 
              (3 + (2*I)*\[Omega]1)*HankelH1[Sqrt[9/4 - mss0], \[Kappa]]))/
            E^(I*\[Delta]*\[Omega]1) + E^(I*\[Delta]*\[Omega]2)*
            (-\[Kappa] + \[Omega]2)*(\[Kappa] + \[Omega]2)*
            (\[Kappa]*HankelH1[-1 + Sqrt[9/4 - mss0], \[Kappa]] - 
             \[Kappa]*HankelH1[1 + Sqrt[9/4 - mss0], \[Kappa]] + 
             (3 - (2*I)*\[Omega]2)*HankelH1[Sqrt[9/4 - mss0], \[Kappa]]) + 
           ((-\[Kappa] + \[Omega]2)*(\[Kappa] + \[Omega]2)*
             (\[Kappa]*HankelH1[-1 + Sqrt[9/4 - mss0], \[Kappa]] - 
              \[Kappa]*HankelH1[1 + Sqrt[9/4 - mss0], \[Kappa]] + 
              (3 + (2*I)*\[Omega]2)*HankelH1[Sqrt[9/4 - mss0], \[Kappa]]))/
            E^(I*\[Delta]*\[Omega]2))*HankelH2[Sqrt[9/4 - mss0], \[Kappa]] - 
         (I*(E^(I*\[Delta]*\[Omega]1)*(-\[Kappa] + \[Omega]1)*
             (\[Kappa] + \[Omega]1)*\[Omega]2*(\[Kappa]*HankelH1[
                -1 + Sqrt[9/4 - mss0], \[Kappa]] - \[Kappa]*HankelH1[
                1 + Sqrt[9/4 - mss0], \[Kappa]] + (3 - (2*I)*\[Omega]1)*
               HankelH1[Sqrt[9/4 - mss0], \[Kappa]]) + 
            ((\[Kappa] - \[Omega]1)*(\[Kappa] + \[Omega]1)*\[Omega]2*
              (\[Kappa]*HankelH1[-1 + Sqrt[9/4 - mss0], \[Kappa]] - \[Kappa]*
                HankelH1[1 + Sqrt[9/4 - mss0], \[Kappa]] + (3 + 
                 (2*I)*\[Omega]1)*HankelH1[Sqrt[9/4 - mss0], \[Kappa]]))/
             E^(I*\[Delta]*\[Omega]1) + E^(I*\[Delta]*\[Omega]2)*\[Omega]1*
             (\[Kappa] - \[Omega]2)*(\[Kappa] + \[Omega]2)*
             (\[Kappa]*HankelH1[-1 + Sqrt[9/4 - mss0], \[Kappa]] - 
              \[Kappa]*HankelH1[1 + Sqrt[9/4 - mss0], \[Kappa]] + 
              (3 - (2*I)*\[Omega]2)*HankelH1[Sqrt[9/4 - mss0], \[Kappa]]) + 
            (\[Omega]1*(-\[Kappa] + \[Omega]2)*(\[Kappa] + \[Omega]2)*
              (\[Kappa]*HankelH1[-1 + Sqrt[9/4 - mss0], \[Kappa]] - \[Kappa]*
                HankelH1[1 + Sqrt[9/4 - mss0], \[Kappa]] + (3 + 
                 (2*I)*\[Omega]2)*HankelH1[Sqrt[9/4 - mss0], \[Kappa]]))/
             E^(I*\[Delta]*\[Omega]2))*(\[Kappa]*HankelH2[-1 + Sqrt[
                9/4 - mss0], \[Kappa]] - \[Kappa]*HankelH2[
              1 + Sqrt[9/4 - mss0], \[Kappa]] + 3*HankelH2[Sqrt[9/4 - mss0], 
              \[Kappa]]))/(\[Omega]1*\[Omega]2))/
        (\[Kappa]*(\[Omega]1^2 - \[Omega]2^2)*
         (HankelH1[Sqrt[9/4 - mss0], \[Kappa]]*
           (HankelH2[-1 + Sqrt[9/4 - mss0], \[Kappa]] - 
            HankelH2[1 + Sqrt[9/4 - mss0], \[Kappa]]) + 
          (-HankelH1[-1 + Sqrt[9/4 - mss0], \[Kappa]] + 
            HankelH1[1 + Sqrt[9/4 - mss0], \[Kappa]])*
           HankelH2[Sqrt[9/4 - mss0], \[Kappa]])), 
     \[Beta]zz -> ((I/4)*E^((2*I)*\[Kappa])*(I*\[Omega]1*\[Omega]2*
          (((\[Kappa]^2 - \[Omega]1^2)*(\[Omega]1 - \[Omega]2)*
             (I + \[Kappa] + \[Omega]2)*(\[Omega]1 + \[Omega]2))/
            E^(I*\[Delta]*\[Omega]2) - ((I + \[Kappa] + \[Omega]1)*
             (\[Omega]1 - \[Omega]2)*(\[Omega]1 + \[Omega]2)*
             (\[Kappa]^2 - \[Omega]2^2))/E^(I*\[Delta]*\[Omega]1) + 
           E^(I*\[Delta]*\[Omega]2)*(\[Kappa]^2 - \[Omega]1^2)*
            (I + \[Kappa] - \[Omega]2)*(\[Omega]1^2 - \[Omega]2^2) + 
           E^(I*\[Delta]*\[Omega]1)*(-I - \[Kappa] + \[Omega]1)*
            (\[Kappa]^2 - \[Omega]2^2)*(\[Omega]1^2 - \[Omega]2^2)) + 
         ((I - \[Kappa])*(\[Omega]1^2 - \[Omega]2^2)*
           (E^(I*\[Delta]*(\[Omega]1 + 2*\[Omega]2))*\[Omega]1*
             (-\[Kappa]^2 + \[Omega]1^2)*(1 - I*\[Kappa] + I*\[Omega]2) + 
            I*E^(I*\[Delta]*\[Omega]1)*\[Omega]1*(-\[Kappa]^2 + \[Omega]1^2)*
             (I + \[Kappa] + \[Omega]2) - I*E^(I*\[Delta]*(2*\[Omega]1 + 
                \[Omega]2))*(-I - \[Kappa] + \[Omega]1)*\[Omega]2*
             (-\[Kappa]^2 + \[Omega]2^2) - I*E^(I*\[Delta]*\[Omega]2)*
             (I + \[Kappa] + \[Omega]1)*\[Omega]2*(-\[Kappa]^2 + 
              \[Omega]2^2)))/E^(I*\[Delta]*(\[Omega]1 + \[Omega]2)) + 
         (2*\[Kappa]*(\[Omega]1^2 - \[Omega]2^2)*
           (E^(I*\[Delta]*(\[Omega]1 + 2*\[Omega]2))*\[Omega]1*
             (-\[Kappa]^2 + \[Omega]1^2)*(1 - I*\[Kappa] + I*\[Omega]2) + 
            I*E^(I*\[Delta]*\[Omega]1)*\[Omega]1*(-\[Kappa]^2 + \[Omega]1^2)*
             (I + \[Kappa] + \[Omega]2) - I*E^(I*\[Delta]*(2*\[Omega]1 + 
                \[Omega]2))*(-I - \[Kappa] + \[Omega]1)*\[Omega]2*
             (-\[Kappa]^2 + \[Omega]2^2) - I*E^(I*\[Delta]*\[Omega]2)*
             (I + \[Kappa] + \[Omega]1)*\[Omega]2*(-\[Kappa]^2 + 
              \[Omega]2^2)))/E^(I*\[Delta]*(\[Omega]1 + \[Omega]2))))/
       (\[Kappa]*\[Omega]1*(\[Omega]1 - \[Omega]2)*\[Omega]2*
        (\[Omega]1 + \[Omega]2)*(\[Omega]1^2 - \[Omega]2^2)), 
     \[Beta]zs -> ((-1)^(1/4)*E^((I/4)*(Sqrt[9 - 4*mss0]*Pi + 4*\[Kappa] - 
           4*\[Delta]*(\[Omega]1 + \[Omega]2)))*Sqrt[Pi/2]*Sqrt[\[Kappa]]*
        \[CapitalOmega]0*(\[Kappa]*(E^(I*\[Delta]*\[Omega]1)*\[Omega]1*
            (I + \[Kappa] - \[Omega]2) + E^(I*\[Delta]*\[Omega]2)*
            (-I - \[Kappa] + \[Omega]1)*\[Omega]2 + 
           E^(I*\[Delta]*(2*\[Omega]1 + \[Omega]2))*(I + \[Kappa] + 
             \[Omega]1)*\[Omega]2 - E^(I*\[Delta]*(\[Omega]1 + 2*\[Omega]2))*
            \[Omega]1*(I + \[Kappa] + \[Omega]2))*
          HankelH1[-1 + Sqrt[9/4 - mss0], \[Kappa]] - 
         \[Kappa]*(E^(I*\[Delta]*\[Omega]1)*\[Omega]1*(I + \[Kappa] - 
             \[Omega]2) + E^(I*\[Delta]*\[Omega]2)*(-I - \[Kappa] + 
             \[Omega]1)*\[Omega]2 + E^(I*\[Delta]*(2*\[Omega]1 + \[Omega]2))*
            (I + \[Kappa] + \[Omega]1)*\[Omega]2 - 
           E^(I*\[Delta]*(\[Omega]1 + 2*\[Omega]2))*\[Omega]1*
            (I + \[Kappa] + \[Omega]2))*HankelH1[1 + Sqrt[9/4 - mss0], 
           \[Kappa]] + (E^(I*\[Delta]*\[Omega]1)*\[Omega]1*
            (I + \[Kappa] - \[Omega]2)*(3 + (2*I)*\[Omega]2) + 
           E^(I*\[Delta]*\[Omega]2)*(3 + (2*I)*\[Omega]1)*(-I - \[Kappa] + 
             \[Omega]1)*\[Omega]2 + E^(I*\[Delta]*(2*\[Omega]1 + \[Omega]2))*
            (3 - (2*I)*\[Omega]1)*(I + \[Kappa] + \[Omega]1)*\[Omega]2 + 
           I*E^(I*\[Delta]*(\[Omega]1 + 2*\[Omega]2))*\[Omega]1*
            (I + \[Kappa] + \[Omega]2)*(3*I + 2*\[Omega]2))*
          HankelH1[Sqrt[9/4 - mss0], \[Kappa]]))/(4*\[Omega]1*\[Omega]2*
        (\[Omega]1^2 - \[Omega]2^2)), \[Beta]sz -> 
      (E^((I/4)*(Pi + Sqrt[9 - 4*mss0]*Pi + 4*\[Kappa]))*
        (I*\[Kappa]*((E^(I*\[Delta]*\[Omega]2)*(\[Kappa]^2 - \[Omega]1^2)*
             (I + \[Kappa] - \[Omega]2)*(-\[Kappa] + \[Omega]2)*
             (\[Kappa] + \[Omega]2))/(\[Omega]2*(-\[Omega]1 + \[Omega]2)*
             (\[Omega]1 + \[Omega]2)) + (E^(I*\[Delta]*\[Omega]1)*
             (I + \[Kappa] - \[Omega]1)*(-\[Kappa] + \[Omega]1)*
             (\[Kappa] + \[Omega]1)*(\[Kappa]^2 - \[Omega]2^2))/
            (\[Omega]1*(\[Omega]1 - \[Omega]2)*(\[Omega]1 + \[Omega]2)) - 
           ((-\[Kappa] + \[Omega]1)*(\[Kappa] + \[Omega]1)*(I + \[Kappa] + 
              \[Omega]1)*(\[Kappa]^2 - \[Omega]2^2))/
            (E^(I*\[Delta]*\[Omega]1)*(\[Omega]1^3 - \[Omega]1*\[Omega]2^
                2)) + ((\[Kappa]^2 - \[Omega]1^2)*(-\[Kappa] + \[Omega]2)*
             (\[Kappa] + \[Omega]2)*(I + \[Kappa] + \[Omega]2))/
            (E^(I*\[Delta]*\[Omega]2)*(\[Omega]1^2*\[Omega]2 - 
              \[Omega]2^3))) + (HankelH1[Sqrt[9/4 - mss0], \[Kappa]]*
           (-2*((E^(I*\[Delta]*\[Omega]2)*(\[Kappa]^2 - \[Omega]1^2)*
                (\[Kappa] - \[Omega]2)*(I + \[Kappa] - \[Omega]2)*
                (\[Kappa] + \[Omega]2))/((-\[Omega]1 + \[Omega]2)*
                (\[Omega]1 + \[Omega]2)) + (E^(I*\[Delta]*\[Omega]1)*
                (\[Kappa] - \[Omega]1)*(I + \[Kappa] - \[Omega]1)*
                (\[Kappa] + \[Omega]1)*(\[Kappa]^2 - \[Omega]2^2))/(
                (\[Omega]1 - \[Omega]2)*(\[Omega]1 + \[Omega]2)) + 
              ((\[Kappa] - \[Omega]1)*(\[Kappa] + \[Omega]1)*(I + \[Kappa] + 
                 \[Omega]1)*(\[Kappa]^2 - \[Omega]2^2))/(E^(I*\[Delta]*
                  \[Omega]1)*(\[Omega]1^2 - \[Omega]2^2)) + 
              ((\[Kappa]^2 - \[Omega]1^2)*(\[Kappa] - \[Omega]2)*
                (\[Kappa] + \[Omega]2)*(I + \[Kappa] + \[Omega]2))/(
                E^(I*\[Delta]*\[Omega]2)*(-\[Omega]1^2 + \[Omega]2^2)))*
             HankelH2[Sqrt[9/4 - mss0], \[Kappa]] - 
            I*((E^(I*\[Delta]*\[Omega]2)*(\[Kappa]^2 - \[Omega]1^2)*
                (I + \[Kappa] - \[Omega]2)*(-\[Kappa] + \[Omega]2)*
                (\[Kappa] + \[Omega]2))/(\[Omega]2*(-\[Omega]1 + \[Omega]2)*
                (\[Omega]1 + \[Omega]2)) + (E^(I*\[Delta]*\[Omega]1)*
                (I + \[Kappa] - \[Omega]1)*(-\[Kappa] + \[Omega]1)*
                (\[Kappa] + \[Omega]1)*(\[Kappa]^2 - \[Omega]2^2))/(\[Omega]1*
                (\[Omega]1 - \[Omega]2)*(\[Omega]1 + \[Omega]2)) - 
              ((-\[Kappa] + \[Omega]1)*(\[Kappa] + \[Omega]1)*(I + \[Kappa] + 
                 \[Omega]1)*(\[Kappa]^2 - \[Omega]2^2))/(E^(I*\[Delta]*
                  \[Omega]1)*(\[Omega]1^3 - \[Omega]1*\[Omega]2^2)) + 
              ((\[Kappa]^2 - \[Omega]1^2)*(-\[Kappa] + \[Omega]2)*
                (\[Kappa] + \[Omega]2)*(I + \[Kappa] + \[Omega]2))/(
                E^(I*\[Delta]*\[Omega]2)*(\[Omega]1^2*\[Omega]2 - 
                 \[Omega]2^3)))*(\[Kappa]*HankelH2[-1 + Sqrt[9/4 - mss0], 
                \[Kappa]] - \[Kappa]*HankelH2[1 + Sqrt[9/4 - mss0], 
                \[Kappa]] + 3*HankelH2[Sqrt[9/4 - mss0], \[Kappa]])))/
          (HankelH1[Sqrt[9/4 - mss0], \[Kappa]]*
            (HankelH2[-1 + Sqrt[9/4 - mss0], \[Kappa]] - 
             HankelH2[1 + Sqrt[9/4 - mss0], \[Kappa]]) + 
           (-HankelH1[-1 + Sqrt[9/4 - mss0], \[Kappa]] + 
             HankelH1[1 + Sqrt[9/4 - mss0], \[Kappa]])*
            HankelH2[Sqrt[9/4 - mss0], \[Kappa]])))/
       (2*Sqrt[2*Pi]*\[Kappa]^(5/2)*\[CapitalOmega]0*
        HankelH2[Sqrt[9/4 - mss0], \[Kappa]]), 
     \[Beta]ss -> ((-1)^(3/4)*E^((I/4)*(Pi + 2*Sqrt[9 - 4*mss0]*Pi + 
           4*\[Delta]*(\[Omega]1 + \[Omega]2)))*
        (\[Kappa]^2*(((\[Kappa]^2 - \[Omega]1^2)*\[Omega]2)/
            E^(I*\[Delta]*(2*\[Omega]1 + \[Omega]2)) + 
           ((-\[Kappa]^2 + \[Omega]1^2)*\[Omega]2)/E^(I*\[Delta]*\[Omega]2) + 
           (\[Omega]1*(\[Kappa]^2 - \[Omega]2^2))/E^(I*\[Delta]*\[Omega]1) + 
           (\[Omega]1*(-\[Kappa]^2 + \[Omega]2^2))/
            E^(I*\[Delta]*(\[Omega]1 + 2*\[Omega]2)))*
          HankelH1[-1 + Sqrt[9/4 - mss0], \[Kappa]]^2 + 
         \[Kappa]^2*(((\[Kappa]^2 - \[Omega]1^2)*\[Omega]2)/
            E^(I*\[Delta]*(2*\[Omega]1 + \[Omega]2)) + 
           ((-\[Kappa]^2 + \[Omega]1^2)*\[Omega]2)/E^(I*\[Delta]*\[Omega]2) + 
           (\[Omega]1*(\[Kappa]^2 - \[Omega]2^2))/E^(I*\[Delta]*\[Omega]1) + 
           (\[Omega]1*(-\[Kappa]^2 + \[Omega]2^2))/
            E^(I*\[Delta]*(\[Omega]1 + 2*\[Omega]2)))*
          HankelH1[1 + Sqrt[9/4 - mss0], \[Kappa]]^2 - 
         2*\[Kappa]*(((\[Kappa]^2 - \[Omega]1^2)*\[Omega]2)/
            E^(I*\[Delta]*(2*\[Omega]1 + \[Omega]2)) + 
           ((-\[Kappa]^2 + \[Omega]1^2)*\[Omega]2)/E^(I*\[Delta]*\[Omega]2) + 
           (\[Omega]1*(\[Kappa]^2 - \[Omega]2^2))/E^(I*\[Delta]*\[Omega]1) + 
           (\[Omega]1*(-\[Kappa]^2 + \[Omega]2^2))/
            E^(I*\[Delta]*(\[Omega]1 + 2*\[Omega]2)))*
          HankelH1[-1 + Sqrt[9/4 - mss0], \[Kappa]]*
          (\[Kappa]*HankelH1[1 + Sqrt[9/4 - mss0], \[Kappa]] - 
           3*HankelH1[Sqrt[9/4 - mss0], \[Kappa]]) - 
         6*\[Kappa]*(((\[Kappa]^2 - \[Omega]1^2)*\[Omega]2)/
            E^(I*\[Delta]*(2*\[Omega]1 + \[Omega]2)) + 
           ((-\[Kappa]^2 + \[Omega]1^2)*\[Omega]2)/E^(I*\[Delta]*\[Omega]2) + 
           (\[Omega]1*(\[Kappa]^2 - \[Omega]2^2))/E^(I*\[Delta]*\[Omega]1) + 
           (\[Omega]1*(-\[Kappa]^2 + \[Omega]2^2))/
            E^(I*\[Delta]*(\[Omega]1 + 2*\[Omega]2)))*
          HankelH1[1 + Sqrt[9/4 - mss0], \[Kappa]]*HankelH1[Sqrt[9/4 - mss0], 
           \[Kappa]] + (-(((\[Kappa]^2 - \[Omega]1^2)*(9 + 4*\[Omega]1^2)*
              \[Omega]2)/E^(I*\[Delta]*\[Omega]2)) + 
           ((\[Kappa]^2 - \[Omega]1^2)*(9 + 4*\[Omega]1^2)*\[Omega]2)/
            E^(I*\[Delta]*(2*\[Omega]1 + \[Omega]2)) + 
           (\[Omega]1*(\[Kappa]^2 - \[Omega]2^2)*(9 + 4*\[Omega]2^2))/
            E^(I*\[Delta]*\[Omega]1) + (\[Omega]1*(-\[Kappa]^2 + \[Omega]2^2)*
             (9 + 4*\[Omega]2^2))/E^(I*\[Delta]*(\[Omega]1 + 2*\[Omega]2)))*
          HankelH1[Sqrt[9/4 - mss0], \[Kappa]]^2))/(4*\[Kappa]*\[Omega]1*
        \[Omega]2*(\[Omega]1^2 - \[Omega]2^2)*
        (HankelH1[Sqrt[9/4 - mss0], \[Kappa]]*
          (-HankelH2[-1 + Sqrt[9/4 - mss0], \[Kappa]] + 
           HankelH2[1 + Sqrt[9/4 - mss0], \[Kappa]]) + 
         (HankelH1[-1 + Sqrt[9/4 - mss0], \[Kappa]] - 
           HankelH1[1 + Sqrt[9/4 - mss0], \[Kappa]])*
          HankelH2[Sqrt[9/4 - mss0], \[Kappa]]))}
 
\[Alpha]sols = {\[Alpha]1 -> (2*k^2*k0^2 + k0^4*mss0 + 
        3*k0^4*\[CapitalOmega]0^2 + k0^4*\[Xi]ss*\[CapitalOmega]0^2 - 
        Sqrt[16*k^2*k0^6*\[CapitalOmega]0^2 + 
          k0^8*(mss0 + (3 + \[Xi]ss)*\[CapitalOmega]0^2)^2])/(2*k0^4), 
     \[Alpha]2 -> (2*k^2*k0^2 + k0^4*mss0 + 3*k0^4*\[CapitalOmega]0^2 + 
        k0^4*\[Xi]ss*\[CapitalOmega]0^2 + 
        Sqrt[16*k^2*k0^6*\[CapitalOmega]0^2 + 
          k0^8*(mss0 + (3 + \[Xi]ss)*\[CapitalOmega]0^2)^2])/(2*k0^4)}
regIIIcoeffsSimp = {\[Alpha]zz -> 
      ((E^(I*Sqrt[\[Alpha]2]*\[Delta])*(-I + Sqrt[\[Alpha]2] - \[Kappa])*
          (\[Alpha]1 - \[Kappa]^2))/(-\[Alpha]1 + \[Alpha]2) + 
        (E^(I*Sqrt[\[Alpha]1]*\[Delta])*(-I + Sqrt[\[Alpha]1] - \[Kappa])*
          (\[Alpha]2 - \[Kappa]^2))/(\[Alpha]1 - \[Alpha]2) + 
        ((I + Sqrt[\[Alpha]2] + \[Kappa])*(-\[Alpha]1 + \[Kappa]^2))/
         (E^(I*Sqrt[\[Alpha]2]*\[Delta])*(-\[Alpha]1 + \[Alpha]2)) + 
        ((I + Sqrt[\[Alpha]1] + \[Kappa])*(-\[Alpha]2 + \[Kappa]^2))/
         (E^(I*Sqrt[\[Alpha]1]*\[Delta])*(\[Alpha]1 - \[Alpha]2)) + 
        (I*(-I + \[Kappa])*(E^(I*(Sqrt[\[Alpha]1] + 2*Sqrt[\[Alpha]2])*
              \[Delta])*Sqrt[\[Alpha]1]*(1 + I*Sqrt[\[Alpha]2] - I*\[Kappa])*
            (\[Alpha]1 - \[Kappa]^2) + I*E^(I*Sqrt[\[Alpha]1]*\[Delta])*
            Sqrt[\[Alpha]1]*(I + Sqrt[\[Alpha]2] + \[Kappa])*
            (\[Alpha]1 - \[Kappa]^2) - I*E^(I*(2*Sqrt[\[Alpha]1] + Sqrt[
                \[Alpha]2])*\[Delta])*Sqrt[\[Alpha]2]*(-I + Sqrt[\[Alpha]1] - 
             \[Kappa])*(\[Alpha]2 - \[Kappa]^2) - 
           I*E^(I*Sqrt[\[Alpha]2]*\[Delta])*Sqrt[\[Alpha]2]*
            (I + Sqrt[\[Alpha]1] + \[Kappa])*(\[Alpha]2 - \[Kappa]^2)))/
         (E^(I*(Sqrt[\[Alpha]1] + Sqrt[\[Alpha]2])*\[Delta])*Sqrt[\[Alpha]1]*
          Sqrt[\[Alpha]2]*(-\[Alpha]1 + \[Alpha]2)))/(4*\[Kappa]), 
     \[Alpha]zs -> ((-1)^(1/4)*E^((I/4)*(Sqrt[9 - 4*mss0]*Pi - 
           4*((Sqrt[\[Alpha]1] + Sqrt[\[Alpha]2])*\[Delta] + \[Kappa])))*
        Sqrt[Pi/2]*Sqrt[\[Kappa]]*\[CapitalOmega]0*
        ((-E^(I*Sqrt[\[Alpha]1]*\[Delta]) + E^(I*(2*Sqrt[\[Alpha]1] + 
              Sqrt[\[Alpha]2])*\[Delta]) - E^(I*(Sqrt[\[Alpha]1] + 
              2*Sqrt[\[Alpha]2])*\[Delta]) + E^(I*Sqrt[\[Alpha]2]*\[Delta]))*
          \[Kappa]*HankelH1[-1 + Sqrt[9/4 - mss0], \[Kappa]] + 
         (E^(I*Sqrt[\[Alpha]1]*\[Delta]) - E^(I*(2*Sqrt[\[Alpha]1] + 
              Sqrt[\[Alpha]2])*\[Delta]) + E^(I*(Sqrt[\[Alpha]1] + 
              2*Sqrt[\[Alpha]2])*\[Delta]) - E^(I*Sqrt[\[Alpha]2]*\[Delta]))*
          \[Kappa]*HankelH1[1 + Sqrt[9/4 - mss0], \[Kappa]] + 
         (E^(I*(2*Sqrt[\[Alpha]1] + Sqrt[\[Alpha]2])*\[Delta])*
            (3 - (2*I)*Sqrt[\[Alpha]1]) + E^(I*Sqrt[\[Alpha]2]*\[Delta])*
            (3 + (2*I)*Sqrt[\[Alpha]1]) + E^(I*Sqrt[\[Alpha]1]*\[Delta])*
            (-3 - (2*I)*Sqrt[\[Alpha]2]) + 
           E^(I*(Sqrt[\[Alpha]1] + 2*Sqrt[\[Alpha]2])*\[Delta])*
            (-3 + (2*I)*Sqrt[\[Alpha]2]))*HankelH1[Sqrt[9/4 - mss0], 
           \[Kappa]] + ((-I + \[Kappa])*(-((E^(I*Sqrt[\[Alpha]1]*\[Delta])*
                Sqrt[\[Alpha]1] - E^(I*(Sqrt[\[Alpha]1] + 2*Sqrt[\[Alpha]2])*
                  \[Delta])*Sqrt[\[Alpha]1] + E^(I*(2*Sqrt[\[Alpha]1] + 
                   Sqrt[\[Alpha]2])*\[Delta])*Sqrt[\[Alpha]2] - 
               E^(I*Sqrt[\[Alpha]2]*\[Delta])*Sqrt[\[Alpha]2])*\[Kappa]*
              HankelH1[-1 + Sqrt[9/4 - mss0], \[Kappa]]) + 
            (E^(I*Sqrt[\[Alpha]1]*\[Delta])*Sqrt[\[Alpha]1] - 
              E^(I*(Sqrt[\[Alpha]1] + 2*Sqrt[\[Alpha]2])*\[Delta])*Sqrt[
                \[Alpha]1] + E^(I*(2*Sqrt[\[Alpha]1] + Sqrt[\[Alpha]2])*
                 \[Delta])*Sqrt[\[Alpha]2] - E^(I*Sqrt[\[Alpha]2]*\[Delta])*
               Sqrt[\[Alpha]2])*\[Kappa]*HankelH1[1 + Sqrt[9/4 - mss0], 
              \[Kappa]] + (E^(I*Sqrt[\[Alpha]1]*\[Delta])*Sqrt[\[Alpha]1]*(
                -3 - (2*I)*Sqrt[\[Alpha]2]) + E^(I*(Sqrt[\[Alpha]1] + 
                  2*Sqrt[\[Alpha]2])*\[Delta])*Sqrt[\[Alpha]1]*(3 - 
                (2*I)*Sqrt[\[Alpha]2]) + E^(I*Sqrt[\[Alpha]2]*\[Delta])*(3 + 
                (2*I)*Sqrt[\[Alpha]1])*Sqrt[\[Alpha]2] + 
              I*E^(I*(2*Sqrt[\[Alpha]1] + Sqrt[\[Alpha]2])*\[Delta])*(3*I + 
                2*Sqrt[\[Alpha]1])*Sqrt[\[Alpha]2])*HankelH1[
              Sqrt[9/4 - mss0], \[Kappa]]))/(Sqrt[\[Alpha]1]*
           Sqrt[\[Alpha]2])))/(4*(\[Alpha]1 - \[Alpha]2)), 
     \[Alpha]sz -> 
      -1/2*(-2*(-(E^(I*Sqrt[\[Alpha]2]*\[Delta])*(Sqrt[\[Alpha]2] - \[Kappa])*
             (I - Sqrt[\[Alpha]2] + \[Kappa])*(Sqrt[\[Alpha]2] + \[Kappa])*
             (\[Alpha]1 - \[Kappa]^2)) + E^(I*Sqrt[\[Alpha]1]*\[Delta])*
            (Sqrt[\[Alpha]1] - \[Kappa])*(I - Sqrt[\[Alpha]1] + \[Kappa])*
            (Sqrt[\[Alpha]1] + \[Kappa])*(\[Alpha]2 - \[Kappa]^2) + 
           ((Sqrt[\[Alpha]1] - \[Kappa])*(Sqrt[\[Alpha]1] + \[Kappa])*
             (I + Sqrt[\[Alpha]1] + \[Kappa])*(\[Alpha]2 - \[Kappa]^2))/
            E^(I*Sqrt[\[Alpha]1]*\[Delta]) + ((Sqrt[\[Alpha]2] - \[Kappa])*
             (Sqrt[\[Alpha]2] + \[Kappa])*(I + Sqrt[\[Alpha]2] + \[Kappa])*
             (-\[Alpha]1 + \[Kappa]^2))/E^(I*Sqrt[\[Alpha]2]*\[Delta]))*
          HankelH2[Sqrt[9/4 - mss0], \[Kappa]] - 
         (I*(E^(I*Sqrt[\[Alpha]2]*\[Delta])*Sqrt[\[Alpha]1]*
             (Sqrt[\[Alpha]2] - \[Kappa])*(I - Sqrt[\[Alpha]2] + \[Kappa])*
             (Sqrt[\[Alpha]2] + \[Kappa])*(\[Alpha]1 - \[Kappa]^2) - 
            (Sqrt[\[Alpha]1]*(Sqrt[\[Alpha]2] - \[Kappa])*(Sqrt[\[Alpha]2] + 
               \[Kappa])*(I + Sqrt[\[Alpha]2] + \[Kappa])*(\[Alpha]1 - 
               \[Kappa]^2))/E^(I*Sqrt[\[Alpha]2]*\[Delta]) + 
            E^(I*Sqrt[\[Alpha]1]*\[Delta])*Sqrt[\[Alpha]2]*(Sqrt[\[Alpha]1] - 
              \[Kappa])*(-I + Sqrt[\[Alpha]1] - \[Kappa])*(Sqrt[\[Alpha]1] + 
              \[Kappa])*(\[Alpha]2 - \[Kappa]^2) + 
            (Sqrt[\[Alpha]2]*(Sqrt[\[Alpha]1] - \[Kappa])*(Sqrt[\[Alpha]1] + 
               \[Kappa])*(I + Sqrt[\[Alpha]1] + \[Kappa])*(\[Alpha]2 - 
               \[Kappa]^2))/E^(I*Sqrt[\[Alpha]1]*\[Delta]))*
           (\[Kappa]*HankelH2[-1 + Sqrt[9/4 - mss0], \[Kappa]] - 
            \[Kappa]*HankelH2[1 + Sqrt[9/4 - mss0], \[Kappa]] + 
            3*HankelH2[Sqrt[9/4 - mss0], \[Kappa]]))/(Sqrt[\[Alpha]1]*
           Sqrt[\[Alpha]2]))/(E^((I/4)*(Pi + Sqrt[9 - 4*mss0]*Pi - 
            4*\[Kappa]))*Sqrt[2*Pi]*(\[Alpha]1 - \[Alpha]2)*\[Kappa]^(5/2)*
         \[CapitalOmega]0*(HankelH1[Sqrt[9/4 - mss0], \[Kappa]]*
           (HankelH2[-1 + Sqrt[9/4 - mss0], \[Kappa]] - 
            HankelH2[1 + Sqrt[9/4 - mss0], \[Kappa]]) + 
          (-HankelH1[-1 + Sqrt[9/4 - mss0], \[Kappa]] + 
            HankelH1[1 + Sqrt[9/4 - mss0], \[Kappa]])*
           HankelH2[Sqrt[9/4 - mss0], \[Kappa]])), 
     \[Alpha]ss -> -1/4*(-2*(E^(I*Sqrt[\[Alpha]1]*\[Delta])*
            (-Sqrt[\[Alpha]1] + \[Kappa])*(Sqrt[\[Alpha]1] + \[Kappa])*
            (\[Kappa]*HankelH1[-1 + Sqrt[9/4 - mss0], \[Kappa]] - 
             \[Kappa]*HankelH1[1 + Sqrt[9/4 - mss0], \[Kappa]] + 
             (3 - (2*I)*Sqrt[\[Alpha]1])*HankelH1[Sqrt[9/4 - mss0], 
               \[Kappa]]) + ((-Sqrt[\[Alpha]1] + \[Kappa])*(Sqrt[\[Alpha]1] + 
              \[Kappa])*(\[Kappa]*HankelH1[-1 + Sqrt[9/4 - mss0], \[Kappa]] - 
              \[Kappa]*HankelH1[1 + Sqrt[9/4 - mss0], \[Kappa]] + 
              (3 + (2*I)*Sqrt[\[Alpha]1])*HankelH1[Sqrt[9/4 - mss0], 
                \[Kappa]]))/E^(I*Sqrt[\[Alpha]1]*\[Delta]) + 
           E^(I*Sqrt[\[Alpha]2]*\[Delta])*(Sqrt[\[Alpha]2] - \[Kappa])*
            (Sqrt[\[Alpha]2] + \[Kappa])*(\[Kappa]*HankelH1[-1 + 
                Sqrt[9/4 - mss0], \[Kappa]] - \[Kappa]*HankelH1[1 + 
                Sqrt[9/4 - mss0], \[Kappa]] + (3 - (2*I)*Sqrt[\[Alpha]2])*
              HankelH1[Sqrt[9/4 - mss0], \[Kappa]]) + 
           ((Sqrt[\[Alpha]2] - \[Kappa])*(Sqrt[\[Alpha]2] + \[Kappa])*
             (\[Kappa]*HankelH1[-1 + Sqrt[9/4 - mss0], \[Kappa]] - 
              \[Kappa]*HankelH1[1 + Sqrt[9/4 - mss0], \[Kappa]] + 
              (3 + (2*I)*Sqrt[\[Alpha]2])*HankelH1[Sqrt[9/4 - mss0], 
                \[Kappa]]))/E^(I*Sqrt[\[Alpha]2]*\[Delta]))*
          HankelH2[Sqrt[9/4 - mss0], \[Kappa]] - 
         (I*(E^(I*Sqrt[\[Alpha]1]*\[Delta])*Sqrt[\[Alpha]2]*
             (Sqrt[\[Alpha]1] - \[Kappa])*(Sqrt[\[Alpha]1] + \[Kappa])*
             (\[Kappa]*HankelH1[-1 + Sqrt[9/4 - mss0], \[Kappa]] - 
              \[Kappa]*HankelH1[1 + Sqrt[9/4 - mss0], \[Kappa]] + 
              (3 - (2*I)*Sqrt[\[Alpha]1])*HankelH1[Sqrt[9/4 - mss0], 
                \[Kappa]]) + (Sqrt[\[Alpha]2]*(-Sqrt[\[Alpha]1] + \[Kappa])*
              (Sqrt[\[Alpha]1] + \[Kappa])*(\[Kappa]*HankelH1[
                 -1 + Sqrt[9/4 - mss0], \[Kappa]] - \[Kappa]*HankelH1[
                 1 + Sqrt[9/4 - mss0], \[Kappa]] + (3 + (2*I)*
                  Sqrt[\[Alpha]1])*HankelH1[Sqrt[9/4 - mss0], \[Kappa]]))/
             E^(I*Sqrt[\[Alpha]1]*\[Delta]) + E^(I*Sqrt[\[Alpha]2]*\[Delta])*
             Sqrt[\[Alpha]1]*(-Sqrt[\[Alpha]2] + \[Kappa])*(Sqrt[\[Alpha]2] + 
              \[Kappa])*(\[Kappa]*HankelH1[-1 + Sqrt[9/4 - mss0], \[Kappa]] - 
              \[Kappa]*HankelH1[1 + Sqrt[9/4 - mss0], \[Kappa]] + 
              (3 - (2*I)*Sqrt[\[Alpha]2])*HankelH1[Sqrt[9/4 - mss0], 
                \[Kappa]]) + (Sqrt[\[Alpha]1]*(Sqrt[\[Alpha]2] - \[Kappa])*
              (Sqrt[\[Alpha]2] + \[Kappa])*(\[Kappa]*HankelH1[
                 -1 + Sqrt[9/4 - mss0], \[Kappa]] - \[Kappa]*HankelH1[
                 1 + Sqrt[9/4 - mss0], \[Kappa]] + (3 + (2*I)*
                  Sqrt[\[Alpha]2])*HankelH1[Sqrt[9/4 - mss0], \[Kappa]]))/
             E^(I*Sqrt[\[Alpha]2]*\[Delta]))*
           (\[Kappa]*HankelH2[-1 + Sqrt[9/4 - mss0], \[Kappa]] - 
            \[Kappa]*HankelH2[1 + Sqrt[9/4 - mss0], \[Kappa]] + 
            3*HankelH2[Sqrt[9/4 - mss0], \[Kappa]]))/(Sqrt[\[Alpha]1]*
           Sqrt[\[Alpha]2]))/((\[Alpha]1 - \[Alpha]2)*\[Kappa]*
         (HankelH1[Sqrt[9/4 - mss0], \[Kappa]]*
           (HankelH2[-1 + Sqrt[9/4 - mss0], \[Kappa]] - 
            HankelH2[1 + Sqrt[9/4 - mss0], \[Kappa]]) + 
          (-HankelH1[-1 + Sqrt[9/4 - mss0], \[Kappa]] + 
            HankelH1[1 + Sqrt[9/4 - mss0], \[Kappa]])*
           HankelH2[Sqrt[9/4 - mss0], \[Kappa]])), 
     \[Beta]zz -> ((I/4)*E^((2*I)*\[Kappa])*
        (((\[Alpha]1 - \[Alpha]2)*(I - \[Kappa])*
           (E^(I*(Sqrt[\[Alpha]1] + 2*Sqrt[\[Alpha]2])*\[Delta])*
             Sqrt[\[Alpha]1]*(1 + I*Sqrt[\[Alpha]2] - I*\[Kappa])*
             (\[Alpha]1 - \[Kappa]^2) + I*E^(I*Sqrt[\[Alpha]1]*\[Delta])*
             Sqrt[\[Alpha]1]*(I + Sqrt[\[Alpha]2] + \[Kappa])*
             (\[Alpha]1 - \[Kappa]^2) - I*E^(I*(2*Sqrt[\[Alpha]1] + 
                Sqrt[\[Alpha]2])*\[Delta])*Sqrt[\[Alpha]2]*
             (-I + Sqrt[\[Alpha]1] - \[Kappa])*(\[Alpha]2 - \[Kappa]^2) - 
            I*E^(I*Sqrt[\[Alpha]2]*\[Delta])*Sqrt[\[Alpha]2]*
             (I + Sqrt[\[Alpha]1] + \[Kappa])*(\[Alpha]2 - \[Kappa]^2)))/
          E^(I*(Sqrt[\[Alpha]1] + Sqrt[\[Alpha]2])*\[Delta]) + 
         (2*(\[Alpha]1 - \[Alpha]2)*\[Kappa]*
           (E^(I*(Sqrt[\[Alpha]1] + 2*Sqrt[\[Alpha]2])*\[Delta])*
             Sqrt[\[Alpha]1]*(1 + I*Sqrt[\[Alpha]2] - I*\[Kappa])*
             (\[Alpha]1 - \[Kappa]^2) + I*E^(I*Sqrt[\[Alpha]1]*\[Delta])*
             Sqrt[\[Alpha]1]*(I + Sqrt[\[Alpha]2] + \[Kappa])*
             (\[Alpha]1 - \[Kappa]^2) - I*E^(I*(2*Sqrt[\[Alpha]1] + 
                Sqrt[\[Alpha]2])*\[Delta])*Sqrt[\[Alpha]2]*
             (-I + Sqrt[\[Alpha]1] - \[Kappa])*(\[Alpha]2 - \[Kappa]^2) - 
            I*E^(I*Sqrt[\[Alpha]2]*\[Delta])*Sqrt[\[Alpha]2]*
             (I + Sqrt[\[Alpha]1] + \[Kappa])*(\[Alpha]2 - \[Kappa]^2)))/
          E^(I*(Sqrt[\[Alpha]1] + Sqrt[\[Alpha]2])*\[Delta]) + 
         I*Sqrt[\[Alpha]1]*Sqrt[\[Alpha]2]*
          (((-Sqrt[\[Alpha]1] + Sqrt[\[Alpha]2])*(Sqrt[\[Alpha]1] + 
              Sqrt[\[Alpha]2])*(I + Sqrt[\[Alpha]2] + \[Kappa])*
             (\[Alpha]1 - \[Kappa]^2))/E^(I*Sqrt[\[Alpha]2]*\[Delta]) + 
           ((Sqrt[\[Alpha]1] - Sqrt[\[Alpha]2])*(Sqrt[\[Alpha]1] + 
              Sqrt[\[Alpha]2])*(I + Sqrt[\[Alpha]1] + \[Kappa])*
             (\[Alpha]2 - \[Kappa]^2))/E^(I*Sqrt[\[Alpha]1]*\[Delta]) + 
           E^(I*Sqrt[\[Alpha]2]*\[Delta])*(\[Alpha]1 - \[Alpha]2)*
            (I - Sqrt[\[Alpha]2] + \[Kappa])*(-\[Alpha]1 + \[Kappa]^2) + 
           E^(I*Sqrt[\[Alpha]1]*\[Delta])*(\[Alpha]1 - \[Alpha]2)*
            (-I + Sqrt[\[Alpha]1] - \[Kappa])*(-\[Alpha]2 + \[Kappa]^2))))/
       (Sqrt[\[Alpha]1]*(\[Alpha]1 - \[Alpha]2)^2*Sqrt[\[Alpha]2]*\[Kappa]), 
     \[Beta]zs -> ((-1)^(1/4)*E^((I/4)*(Sqrt[9 - 4*mss0]*Pi - 
           4*(Sqrt[\[Alpha]1] + Sqrt[\[Alpha]2])*\[Delta] + 4*\[Kappa]))*
        Sqrt[Pi/2]*Sqrt[\[Kappa]]*\[CapitalOmega]0*
        (\[Kappa]*(E^(I*Sqrt[\[Alpha]2]*\[Delta])*Sqrt[\[Alpha]2]*
            (-I + Sqrt[\[Alpha]1] - \[Kappa]) + 
           E^(I*(2*Sqrt[\[Alpha]1] + Sqrt[\[Alpha]2])*\[Delta])*
            Sqrt[\[Alpha]2]*(I + Sqrt[\[Alpha]1] + \[Kappa]) + 
           E^(I*Sqrt[\[Alpha]1]*\[Delta])*Sqrt[\[Alpha]1]*
            (I - Sqrt[\[Alpha]2] + \[Kappa]) - 
           E^(I*(Sqrt[\[Alpha]1] + 2*Sqrt[\[Alpha]2])*\[Delta])*
            Sqrt[\[Alpha]1]*(I + Sqrt[\[Alpha]2] + \[Kappa]))*
          HankelH1[-1 + Sqrt[9/4 - mss0], \[Kappa]] - 
         \[Kappa]*(E^(I*Sqrt[\[Alpha]2]*\[Delta])*Sqrt[\[Alpha]2]*
            (-I + Sqrt[\[Alpha]1] - \[Kappa]) + 
           E^(I*(2*Sqrt[\[Alpha]1] + Sqrt[\[Alpha]2])*\[Delta])*
            Sqrt[\[Alpha]2]*(I + Sqrt[\[Alpha]1] + \[Kappa]) + 
           E^(I*Sqrt[\[Alpha]1]*\[Delta])*Sqrt[\[Alpha]1]*
            (I - Sqrt[\[Alpha]2] + \[Kappa]) - 
           E^(I*(Sqrt[\[Alpha]1] + 2*Sqrt[\[Alpha]2])*\[Delta])*
            Sqrt[\[Alpha]1]*(I + Sqrt[\[Alpha]2] + \[Kappa]))*
          HankelH1[1 + Sqrt[9/4 - mss0], \[Kappa]] + 
         (E^(I*Sqrt[\[Alpha]2]*\[Delta])*(3 + (2*I)*Sqrt[\[Alpha]1])*
            Sqrt[\[Alpha]2]*(-I + Sqrt[\[Alpha]1] - \[Kappa]) + 
           E^(I*(2*Sqrt[\[Alpha]1] + Sqrt[\[Alpha]2])*\[Delta])*
            (3 - (2*I)*Sqrt[\[Alpha]1])*Sqrt[\[Alpha]2]*
            (I + Sqrt[\[Alpha]1] + \[Kappa]) + E^(I*Sqrt[\[Alpha]1]*\[Delta])*
            Sqrt[\[Alpha]1]*(3 + (2*I)*Sqrt[\[Alpha]2])*
            (I - Sqrt[\[Alpha]2] + \[Kappa]) + 
           I*E^(I*(Sqrt[\[Alpha]1] + 2*Sqrt[\[Alpha]2])*\[Delta])*
            Sqrt[\[Alpha]1]*(3*I + 2*Sqrt[\[Alpha]2])*(I + Sqrt[\[Alpha]2] + 
             \[Kappa]))*HankelH1[Sqrt[9/4 - mss0], \[Kappa]]))/
       (4*Sqrt[\[Alpha]1]*(\[Alpha]1 - \[Alpha]2)*Sqrt[\[Alpha]2]), 
     \[Beta]sz -> (E^((I/4)*(Pi + Sqrt[9 - 4*mss0]*Pi + 4*\[Kappa]))*
        ((I*\[Kappa]*(E^(I*Sqrt[\[Alpha]2]*\[Delta])*Sqrt[\[Alpha]1]*
             (Sqrt[\[Alpha]2] - \[Kappa])*(I - Sqrt[\[Alpha]2] + \[Kappa])*
             (Sqrt[\[Alpha]2] + \[Kappa])*(\[Alpha]1 - \[Kappa]^2) - 
            (Sqrt[\[Alpha]1]*(Sqrt[\[Alpha]2] - \[Kappa])*(Sqrt[\[Alpha]2] + 
               \[Kappa])*(I + Sqrt[\[Alpha]2] + \[Kappa])*(\[Alpha]1 - 
               \[Kappa]^2))/E^(I*Sqrt[\[Alpha]2]*\[Delta]) + 
            E^(I*Sqrt[\[Alpha]1]*\[Delta])*Sqrt[\[Alpha]2]*(Sqrt[\[Alpha]1] - 
              \[Kappa])*(-I + Sqrt[\[Alpha]1] - \[Kappa])*(Sqrt[\[Alpha]1] + 
              \[Kappa])*(\[Alpha]2 - \[Kappa]^2) + 
            (Sqrt[\[Alpha]2]*(Sqrt[\[Alpha]1] - \[Kappa])*(Sqrt[\[Alpha]1] + 
               \[Kappa])*(I + Sqrt[\[Alpha]1] + \[Kappa])*(\[Alpha]2 - 
               \[Kappa]^2))/E^(I*Sqrt[\[Alpha]1]*\[Delta])))/
          (Sqrt[\[Alpha]1]*Sqrt[\[Alpha]2]) + 
         (HankelH1[Sqrt[9/4 - mss0], \[Kappa]]*
           (-2*(-(E^(I*Sqrt[\[Alpha]2]*\[Delta])*(Sqrt[\[Alpha]2] - \[Kappa])*
                (I - Sqrt[\[Alpha]2] + \[Kappa])*(Sqrt[\[Alpha]2] + \[Kappa])*
                (\[Alpha]1 - \[Kappa]^2)) + E^(I*Sqrt[\[Alpha]1]*\[Delta])*(
                Sqrt[\[Alpha]1] - \[Kappa])*(I - Sqrt[\[Alpha]1] + \[Kappa])*(
                Sqrt[\[Alpha]1] + \[Kappa])*(\[Alpha]2 - \[Kappa]^2) + 
              ((Sqrt[\[Alpha]1] - \[Kappa])*(Sqrt[\[Alpha]1] + \[Kappa])*
                (I + Sqrt[\[Alpha]1] + \[Kappa])*(\[Alpha]2 - \[Kappa]^2))/E^
                (I*Sqrt[\[Alpha]1]*\[Delta]) + ((Sqrt[\[Alpha]2] - \[Kappa])*
                (Sqrt[\[Alpha]2] + \[Kappa])*(I + Sqrt[\[Alpha]2] + \[Kappa])*
                (-\[Alpha]1 + \[Kappa]^2))/E^(I*Sqrt[\[Alpha]2]*\[Delta]))*
             HankelH2[Sqrt[9/4 - mss0], \[Kappa]] - 
            (I*(E^(I*Sqrt[\[Alpha]2]*\[Delta])*Sqrt[\[Alpha]1]*
                (Sqrt[\[Alpha]2] - \[Kappa])*(I - Sqrt[\[Alpha]2] + \[Kappa])*
                (Sqrt[\[Alpha]2] + \[Kappa])*(\[Alpha]1 - \[Kappa]^2) - 
               (Sqrt[\[Alpha]1]*(Sqrt[\[Alpha]2] - \[Kappa])*
                 (Sqrt[\[Alpha]2] + \[Kappa])*(I + Sqrt[\[Alpha]2] + 
                  \[Kappa])*(\[Alpha]1 - \[Kappa]^2))/E^(I*Sqrt[\[Alpha]2]*
                  \[Delta]) + E^(I*Sqrt[\[Alpha]1]*\[Delta])*Sqrt[\[Alpha]2]*
                (Sqrt[\[Alpha]1] - \[Kappa])*(-I + Sqrt[\[Alpha]1] - 
                 \[Kappa])*(Sqrt[\[Alpha]1] + \[Kappa])*(\[Alpha]2 - 
                 \[Kappa]^2) + (Sqrt[\[Alpha]2]*(Sqrt[\[Alpha]1] - \[Kappa])*
                 (Sqrt[\[Alpha]1] + \[Kappa])*(I + Sqrt[\[Alpha]1] + 
                  \[Kappa])*(\[Alpha]2 - \[Kappa]^2))/E^(I*Sqrt[\[Alpha]1]*
                  \[Delta]))*(\[Kappa]*HankelH2[-1 + Sqrt[9/4 - mss0], 
                 \[Kappa]] - \[Kappa]*HankelH2[1 + Sqrt[9/4 - mss0], 
                 \[Kappa]] + 3*HankelH2[Sqrt[9/4 - mss0], \[Kappa]]))/
             (Sqrt[\[Alpha]1]*Sqrt[\[Alpha]2])))/
          (HankelH1[Sqrt[9/4 - mss0], \[Kappa]]*
            (HankelH2[-1 + Sqrt[9/4 - mss0], \[Kappa]] - 
             HankelH2[1 + Sqrt[9/4 - mss0], \[Kappa]]) + 
           (-HankelH1[-1 + Sqrt[9/4 - mss0], \[Kappa]] + 
             HankelH1[1 + Sqrt[9/4 - mss0], \[Kappa]])*
            HankelH2[Sqrt[9/4 - mss0], \[Kappa]])))/
       (2*Sqrt[2*Pi]*(\[Alpha]1 - \[Alpha]2)*\[Kappa]^(5/2)*\[CapitalOmega]0*
        HankelH2[Sqrt[9/4 - mss0], \[Kappa]]), 
     \[Beta]ss -> ((-1)^(3/4)*E^((I/4)*(Pi + 2*Sqrt[9 - 4*mss0]*Pi + 
           4*(Sqrt[\[Alpha]1] + Sqrt[\[Alpha]2])*\[Delta]))*
        (\[Kappa]^2*((Sqrt[\[Alpha]2]*(\[Alpha]1 - \[Kappa]^2))/
            E^(I*Sqrt[\[Alpha]2]*\[Delta]) + (Sqrt[\[Alpha]1]*
             (\[Alpha]2 - \[Kappa]^2))/E^(I*(Sqrt[\[Alpha]1] + 2*
                Sqrt[\[Alpha]2])*\[Delta]) + (Sqrt[\[Alpha]2]*
             (-\[Alpha]1 + \[Kappa]^2))/E^(I*(2*Sqrt[\[Alpha]1] + Sqrt[
                \[Alpha]2])*\[Delta]) + (Sqrt[\[Alpha]1]*(-\[Alpha]2 + 
              \[Kappa]^2))/E^(I*Sqrt[\[Alpha]1]*\[Delta]))*
          HankelH1[-1 + Sqrt[9/4 - mss0], \[Kappa]]^2 + 
         \[Kappa]^2*((Sqrt[\[Alpha]2]*(\[Alpha]1 - \[Kappa]^2))/
            E^(I*Sqrt[\[Alpha]2]*\[Delta]) + (Sqrt[\[Alpha]1]*
             (\[Alpha]2 - \[Kappa]^2))/E^(I*(Sqrt[\[Alpha]1] + 2*
                Sqrt[\[Alpha]2])*\[Delta]) + (Sqrt[\[Alpha]2]*
             (-\[Alpha]1 + \[Kappa]^2))/E^(I*(2*Sqrt[\[Alpha]1] + Sqrt[
                \[Alpha]2])*\[Delta]) + (Sqrt[\[Alpha]1]*(-\[Alpha]2 + 
              \[Kappa]^2))/E^(I*Sqrt[\[Alpha]1]*\[Delta]))*
          HankelH1[1 + Sqrt[9/4 - mss0], \[Kappa]]^2 - 
         2*\[Kappa]*((Sqrt[\[Alpha]2]*(\[Alpha]1 - \[Kappa]^2))/
            E^(I*Sqrt[\[Alpha]2]*\[Delta]) + (Sqrt[\[Alpha]1]*
             (\[Alpha]2 - \[Kappa]^2))/E^(I*(Sqrt[\[Alpha]1] + 2*
                Sqrt[\[Alpha]2])*\[Delta]) + (Sqrt[\[Alpha]2]*
             (-\[Alpha]1 + \[Kappa]^2))/E^(I*(2*Sqrt[\[Alpha]1] + Sqrt[
                \[Alpha]2])*\[Delta]) + (Sqrt[\[Alpha]1]*(-\[Alpha]2 + 
              \[Kappa]^2))/E^(I*Sqrt[\[Alpha]1]*\[Delta]))*
          HankelH1[-1 + Sqrt[9/4 - mss0], \[Kappa]]*
          (\[Kappa]*HankelH1[1 + Sqrt[9/4 - mss0], \[Kappa]] - 
           3*HankelH1[Sqrt[9/4 - mss0], \[Kappa]]) - 
         6*\[Kappa]*((Sqrt[\[Alpha]2]*(\[Alpha]1 - \[Kappa]^2))/
            E^(I*Sqrt[\[Alpha]2]*\[Delta]) + (Sqrt[\[Alpha]1]*
             (\[Alpha]2 - \[Kappa]^2))/E^(I*(Sqrt[\[Alpha]1] + 2*
                Sqrt[\[Alpha]2])*\[Delta]) + (Sqrt[\[Alpha]2]*
             (-\[Alpha]1 + \[Kappa]^2))/E^(I*(2*Sqrt[\[Alpha]1] + Sqrt[
                \[Alpha]2])*\[Delta]) + (Sqrt[\[Alpha]1]*(-\[Alpha]2 + 
              \[Kappa]^2))/E^(I*Sqrt[\[Alpha]1]*\[Delta]))*
          HankelH1[1 + Sqrt[9/4 - mss0], \[Kappa]]*HankelH1[Sqrt[9/4 - mss0], 
           \[Kappa]] + (((9 + 4*\[Alpha]1)*Sqrt[\[Alpha]2]*(\[Alpha]1 - 
              \[Kappa]^2))/E^(I*Sqrt[\[Alpha]2]*\[Delta]) + 
           (Sqrt[\[Alpha]1]*(9 + 4*\[Alpha]2)*(\[Alpha]2 - \[Kappa]^2))/
            E^(I*(Sqrt[\[Alpha]1] + 2*Sqrt[\[Alpha]2])*\[Delta]) + 
           ((9 + 4*\[Alpha]1)*Sqrt[\[Alpha]2]*(-\[Alpha]1 + \[Kappa]^2))/
            E^(I*(2*Sqrt[\[Alpha]1] + Sqrt[\[Alpha]2])*\[Delta]) + 
           (Sqrt[\[Alpha]1]*(9 + 4*\[Alpha]2)*(-\[Alpha]2 + \[Kappa]^2))/
            E^(I*Sqrt[\[Alpha]1]*\[Delta]))*HankelH1[Sqrt[9/4 - mss0], 
            \[Kappa]]^2))/(4*Sqrt[\[Alpha]1]*(\[Alpha]1 - \[Alpha]2)*
        Sqrt[\[Alpha]2]*\[Kappa]*(HankelH1[Sqrt[9/4 - mss0], \[Kappa]]*
          (-HankelH2[-1 + Sqrt[9/4 - mss0], \[Kappa]] + 
           HankelH2[1 + Sqrt[9/4 - mss0], \[Kappa]]) + 
         (HankelH1[-1 + Sqrt[9/4 - mss0], \[Kappa]] - 
           HankelH1[1 + Sqrt[9/4 - mss0], \[Kappa]])*
          HankelH2[Sqrt[9/4 - mss0], \[Kappa]]))}
 
\[Alpha]sols = {\[Alpha]1 -> (2*k^2*k0^2 + k0^4*mss0 + 
        3*k0^4*\[CapitalOmega]0^2 + k0^4*\[Xi]ss*\[CapitalOmega]0^2 - 
        Sqrt[16*k^2*k0^6*\[CapitalOmega]0^2 + 
          k0^8*(mss0 + (3 + \[Xi]ss)*\[CapitalOmega]0^2)^2])/(2*k0^4), 
     \[Alpha]2 -> (2*k^2*k0^2 + k0^4*mss0 + 3*k0^4*\[CapitalOmega]0^2 + 
        k0^4*\[Xi]ss*\[CapitalOmega]0^2 + 
        Sqrt[16*k^2*k0^6*\[CapitalOmega]0^2 + 
          k0^8*(mss0 + (3 + \[Xi]ss)*\[CapitalOmega]0^2)^2])/(2*k0^4)}
