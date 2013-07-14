package org.bosik.compensation.persistence.repository.common;

/**
 * Интерфейс репозитория.
 * 
 * @author Bosik
 * 
 * @param <T>
 *            Тип базы
 */
public interface BaseRepository<T>
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
	public T getBase();

	/**
	 * Сохраняет базу в репозитории
	 * 
	 * @param base
	 */
	public void postBase(T base);

}
