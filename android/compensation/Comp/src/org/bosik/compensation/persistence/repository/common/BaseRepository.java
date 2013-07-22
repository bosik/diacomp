package org.bosik.compensation.persistence.repository.common;

/**
 * Интерфейс репозитория.
 * 
 * @author Bosik
 * 
 * @param <T>
 *            Тип базы
 */
@Deprecated
public interface BaseRepository<T>
{
	/**
	 * Получает номер версии базы
	 * 
	 * @return Номер версии
	 */
	public int getVersion();

	/**
	 * Получает базу из репозитория.
	 * 
	 * @return База или null, если она не существует.
	 */
	public T getBase();

	/**
	 * Сохраняет базу в репозитории
	 * 
	 * @param base
	 */
	public void postBase(T base);

}
