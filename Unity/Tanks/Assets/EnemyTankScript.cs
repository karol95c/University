using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.AI;
using System;
using UnityEngine.SceneManagement;

public class EnemyTankScript : TankScript {

	// Use this for initialization
	private static Vector3[] enemyRespawn = new Vector3[3];
	public GameObject enemyExplodePrefab;
	public Transform enemyExplodeSpawn;
	static GameObject mapGO;
	static GameObject tankGO;
	private static int nextID = 0;
	private static int lives = 7;
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
	int r;



	void Start () {
		rigidBody = GetComponent<Rigidbody>();
		tankAudio = GetComponents<AudioSource>();
		mapGO = GameObject.Find("Map");
		tankGO = GameObject.Find("Tank");
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
		r = getrandom.Next(6);
		setTarget();
		mainThrust = 80f;
		torch = transform.Find("Light").GetComponent<Light>();
		nightModeOn = MapScript.isNightModeOn();
		torchManage();
		gm = GameObject.Find("GameManager").GetComponent<GameManagerSc>();

	}
	// Update is called once per frame

	public static void restartLives()
	{
		EnemyTankScript.lives = 7;
	}
	void Update () {
		// rotateEnemy(90);
		direction = transform.TransformDirection(target);
		distance = Vector3.Distance(target, transform.position);
		if(ammoReloaded) StartCoroutine(Wait((UnityEngine.Random.value * 2.0f) + 2f));
		if (distance > 6f)
		{
			// agent.isStopped = true;
			// LookToward(destination, distance);
			// distanceBasedSpeedModifier = GetSpeedModifier(distance);
			
			movement = transform.forward * Time.deltaTime * 0.03f;
			agent.Move(movement);
		}
		agent.SetDestination(target);
		
	}
	private void torchManage()
	{
		if (!nightModeOn)
		{
			torch.enabled = false;
		}
	}
	public static int getLives()
	{
		return lives;
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
		gm.enemyDestroyed();
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
 
