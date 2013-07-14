package org.bosik.compensation.persistence.repository.foodbase;

public interface FoodBaseRepository
{
	/**
	 * Получает номер версии базы 
	 * @return Номер версии
	 */
	public int getVersion();
	
	public List<Food>
}
