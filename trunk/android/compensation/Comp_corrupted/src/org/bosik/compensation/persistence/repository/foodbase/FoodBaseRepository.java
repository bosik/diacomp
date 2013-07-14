package org.bosik.compensation.persistence.repository.foodbase;

import org.bosik.compensation.persistence.entity.foodbase.FoodBase;

public interface FoodBaseRepository
{
	/**
	 * Получает номер версии базы
	 * 
	 * @return Номер версии
	 */
	public int getVersion();

	/**
	 * Получает базу из репозитория
	 * 
	 * @return
	 */
	public FoodBase getBase();

	/**
	 * Сохраняет базу в репозитории
	 * 
	 * @param base
	 */
	public void postBase(FoodBase base);

}
