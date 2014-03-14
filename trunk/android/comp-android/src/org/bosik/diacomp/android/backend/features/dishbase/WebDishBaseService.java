package org.bosik.diacomp.android.backend.features.dishbase;

import org.bosik.diacomp.android.backend.common.webclient.WebClient;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.persistence.serializers.Serializer;

public class WebDishBaseService// implements DishBaseService
{
	private final WebClient							webClient;
	private final Serializer<Versioned<DishItem>>	serializer;

	public WebDishBaseService(WebClient webClient, Serializer<Versioned<DishItem>> serializer)
	{
		this.webClient = webClient;
		this.serializer = serializer;
	}

	// TODO: implement
}
