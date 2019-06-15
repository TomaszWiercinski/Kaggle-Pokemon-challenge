prepare_data <- function(pokemon, combats)
{
  # join pokemon and combats
  pokemon <- pokemon %>% rename(First_pokemon = X.)
  combats_full <- merge(combats, pokemon, by='First_pokemon')
  pokemon <- pokemon %>% rename(Second_pokemon = First_pokemon)
  combats_full <- merge(combats_full, pokemon, by='Second_pokemon')
  
  # simplify stats
  combats_full <- combats_full %>% mutate(Attack.Diff = Attack.x-Attack.y, 
                                          HP.Diff = HP.x-HP.y,
                                          Defense.Diff=Defense.x-Defense.y, 
                                          Sp.Atk.Diff = Sp..Atk.x-Sp..Atk.y, 
                                          Sp.Def.Diff = Sp..Def.x-Sp..Def.y,
                                          Speed.Diff = Speed.x-Speed.y)
  # add type multipliers
  combats_full <- combats_full %>% 
    rowwise() %>% 
    mutate(Mult = multipliers[Type.1.x, Type.1.y] *
             multipliers[Type.1.x, Type.2.y] *
             multipliers[Type.2.x, Type.1.y] *
             multipliers[Type.2.x, Type.2.y]) %>% 
    ungroup()
  
  # add attacking power = Attack / HP
  combats_full <- combats_full %>%
    mutate(Attack.Pow.x = Attack.x / (HP.y + Defense.y),
           Attack.Pow.y = Attack.y / (HP.x + Defense.x))
  
  return(combats_full)
}

simplify_winner <- function(combats_full)
{
  combats_full <- combats_full %>% mutate(winner_xy = ifelse(First_pokemon == Winner, 'x', 'y'))
  combats_full$winner_xy <- factor(combats_full$winner_xy)
  return(combats_full)
}