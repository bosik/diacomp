package org.bosik.diacomp.android.backend.features.foodbase;

import org.bosik.diacomp.android.backend.common.webclient.WebClient;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.persistence.serializers.Serializer;

public class WebFoodBaseService// implements FoodBaseService
{
	// private static final String TAG = WebFoodBaseService.class.getSimpleName();

	private final WebClient							webClient;
	private final Serializer<Versioned<FoodItem>>	serializer;

	public WebFoodBaseService(WebClient webClient, Serializer<Versioned<FoodItem>> serializer)
	{
		this.webClient = webClient;
		this.serializer = serializer;
	}

	// TODO: implement when interface is ready
}
