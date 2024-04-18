package br.com.sbk.sbking.core.rulesets.concrete;

import br.com.sbk.sbking.core.Suit;
import br.com.sbk.sbking.core.comparators.CardInsideHandWithSuitComparator;
import br.com.sbk.sbking.core.rulesets.implementations.DefaultSuitFollowable;
import br.com.sbk.sbking.core.rulesets.implementations.TrumpSuitWinnable;

public class PositiveWithTrumpsRuleset extends PositiveRuleset {

    /**
     * @deprecated Kryo needs a no-arg constructor
     */
    @Deprecated
    @SuppressWarnings("unused")
    private PositiveWithTrumpsRuleset() {
    }

    private Suit trumpSuit;

    public PositiveWithTrumpsRuleset(Suit trumpSuit) {
        super();
        this.suitFollowable = new DefaultSuitFollowable();
        this.winnable = new TrumpSuitWinnable(trumpSuit);
        this.trumpSuit = trumpSuit;
        this.cardComparator = new CardInsideHandWithSuitComparator(this.trumpSuit);
    }

    public Suit getTrumpSuit() {
        return trumpSuit;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((trumpSuit == null) ? 0 : trumpSuit.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (!super.equals(obj)) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        PositiveWithTrumpsRuleset other = (PositiveWithTrumpsRuleset) obj;
        if (trumpSuit != other.trumpSuit) {
            return false;
        }
        return true;
    }

}
