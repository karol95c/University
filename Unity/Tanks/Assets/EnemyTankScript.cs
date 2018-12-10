using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.AI;
using System;

public class EnemyTankScript : TankScript {

	// Use this for initialization
	private static Vector3[] enemyRespawn = new Vector3[3];
	public GameObject enemyExplodePrefab;
	public Transform enemyExplodeSpawn;
	static GameObject mapGO;
	private static int nextID = 0;
	private static int lives = 9;
	private int enemyID;

	private bool rotationInProgress = false;
	private bool moveInProgress = false;
	float rotate = 0f;

	private static readonly System.Random getrandom = new System.Random();
	Vector3 targetRotation;
	Vector3 initialRotation;

    // Angular speed in degrees per sec.
    float speed = 10f;
	Vector3 targetPosition;
	Transform thisTransform;
	NavMeshAgent agent;
	Vector3 target;
	Vector3 direction;
	float distance;
	float distanceBasedSpeedModifier;
	Vector3 movement;
	string debug;


	void Start () {
		rigidBody = GetComponent<Rigidbody>();
		tankAudio = GetComponents<AudioSource>();
		mapGO = GameObject.Find("Map");
		thisTransform = GetComponent<Transform>();
		setEnemyResp();

		enemyID = nextID;
		if ( nextID < 2)
		{
			nextID++;
		}
		ammo = 100;
		ammoReloaded = true;
		agent = GetComponent<NavMeshAgent>();
		setTarget();

	}
	// Update is called once per frame
	void Update () {
		// rotateEnemy(90);
		direction = transform.TransformDirection(target);
		distance = Vector3.Distance(target, transform.position);
		if(ammoReloaded) StartCoroutine(Wait(2f));
		if (distance > 5f)
		{
			// agent.isStopped = true;
			// LookToward(destination, distance);
			// distanceBasedSpeedModifier = GetSpeedModifier(distance);
			
			movement = transform.forward * Time.deltaTime * 0.05f;
			agent.Move(movement);
		}
		agent.SetDestination(target);
		
	}
	
	void setTarget()
	{
		target = mapGO.GetComponent<MapScript>().basePosition;
	}
	public void moveToPoint(Vector3 destination)
	{
		this.target = destination;
		agent.isStopped = false;
		agent.enabled = true;
		debug = destination.ToString();

		// agent.SetDestination(target);
	}
	// private void rotateEnemy()
	// {
	// 	if (rotationInProgress && !moveInProgress)
	// 	{
	// 		if (Math.Abs(transform.rotation.y - targetRotation.y) < 0.000005f)
	// 		{
	// 			rotationInProgress = false;
	// 			moveInProgress = false;
	// 		}
	// 		else 
	// 		{
	// 			transform.rotation = Quaternion.RotateTowards(transform.rotation, targetRotation, 20 * Time.deltaTime);
	// 		}
	// 	}
	// 	else 
	// 	{
	// 		// targetRotation = Quaternion.Euler(0f, transform.rotation.y + rotate, 0f);
	// 		rotationInProgress = true;
	// 		moveInProgress = false;
	// 	}
	// }

	// private void moveForward()
	// {

	// 	if (!rotationInProgress && moveInProgress)
	// 	{
	// 		if (Math.Abs(transform.position.z - targetPosition.z) < 0.00005f || Math.Abs(transform.position.x - targetPosition.x) < 0.00005f)
	// 		{
	// 			rotationInProgress = false;
	// 			moveInProgress = false;
	// 		}
	// 		else 
	// 		{
	// 			transform.position = Vector3.MoveTowards(transform.position, targetPosition, 1 * Time.deltaTime);
	// 		}
	// 	}
	// 	else 
	// 	{
	// 		targetPosition = moveDirection();
	// 		rotationInProgress = false;
	// 		moveInProgress = true;
	// 	}
		
		
	// }
	public static void setNextID(int id)
	{
		nextID = id;
	}
	public int getID()
	{
		return enemyID;
	}
	public void Explode()
	{
		var enemyExplode = (GameObject)Instantiate (
			enemyExplodePrefab,
			enemyExplodeSpawn.position,
			enemyExplodeSpawn.rotation);
		enemyExplode.GetComponent<ParticleSystem>().Play();
		enemyExplode.GetComponent<AudioSource>().Play();
		Destroy(enemyExplode, 1.0f);
		lives --;
		Destroy(gameObject);
	
	}
	public static void setEnemyResp()
	{
		MapScript mapScript = mapGO.GetComponent<MapScript>();
		int mapSize = mapScript.getMapSize();
		int mapCenter = mapSize / 2;
		enemyRespawn[1] = new Vector3(mapCenter, 0.5f, mapSize - 2);
		enemyRespawn[0] = new Vector3(1f, 0.5f, mapSize - 2);
		enemyRespawn[2] = new Vector3(mapSize - 2, 0.5f, mapSize - 2);
	}
	
	public static Vector3? canCreateTank()
	{
		Debug.Log(nextID);
		if (lives > 0)
		{
			return enemyRespawn[nextID];
		}
		return null;
	}

	IEnumerator Wait(float duration)
    {
        //This is a coroutine
		ammoReloaded = false;
        yield return new WaitForSeconds(duration);   //Wait
		Fire();
		ammoReloaded = true;
    }


	// Vector3 moveDirection()
	// {
	// 	float rotation = thisTransform.rotation.y;
	// 	float x;
	// 	float z;
	// 	if ( Math.Abs(rotation - 90f) < 1f)
	// 	{
	// 		// return new Vector3(-1f, 0f, 0f);
	// 		x = -1f;
	// 		z = 0f;
	// 	}
	// 	else if(Math.Abs(rotation) < 1f)
	// 	{
	// 		// transform.rotation = Quaternion.Euler(thisTransform.rotation.x, 0f, thisTransform.rotation.z);
	// 		// return new Vector3(0f, 0f, -1f);
	// 		x = 0f;
	// 		z = -1f;
	// 	}
	// 	else if( Math.Abs(rotation - 270f) < 1f)
	// 	{
	// 		// transform.rotation = Quaternion.Euler(thisTransform.rotation.x, 270f, thisTransform.rotation.z);
	// 		x = -1f;
	// 		z = 0f;
	// 		// return new Vector3(-1f, 0f, 0f);
	// 	}
	// 	else
	// 	{
	// 		// transform.rotation = Quaternion.Euler(thisTransform.rotation.x, 180f, thisTransform.rotation.z);
	// 		x = 0f;
	// 		z = 1f;
	// 		// return new Vector3(0f, 0f, 1f);
	// 	}
	// 	return new Vector3(transform.position.x + x, transform.position.y, transform.position.z + z);
	// }

	void OnCollisionEnter(Collision col)
	{
		string tag = col.gameObject.tag;
		if (tag == "Brick")
		{
			
			Fire();
			if (!agent.isStopped)
			{
				initialRotation = transform.position;
				targetRotation = col.gameObject.transform.position;
				lookAtMove();
				
			}
		}

	}

	void lookAtMove()
	{
		agent.isStopped = true;
		transform.position = Vector3.Slerp(initialRotation, targetRotation, Time.deltaTime* 1f);
		agent.isStopped = false;

	}
}
 
